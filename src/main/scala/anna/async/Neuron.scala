package anna.async

import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors, TimerScheduler}
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal => AkkaSignal}
import anna.Context
import anna.async.Messages.{Answer, Msg, NeuronInfo, Success}
import anna.async.NeuronTriggers.Trigger
import anna.async.Neuron._
import anna.logger.LOG
import anna.utils.Utils

import scala.concurrent.duration._

/**
 * A neuron actor, implemented in the object-oriented `AbstractBehavior` style:
 *
 *  - The message protocol is an explicit sealed `Command` hierarchy (see the
 *    companion object); every request carries an explicit `replyTo: ActorRef[_]`.
 *  - The active/silent state switch is expressed by returning a different
 *    `Behavior` (`silentBehavior` for the silent state, `this` for the active
 *    state).
 *  - Scheduling uses `Behaviors.withTimers`, sending the internal `WakeUp` /
 *    `WakeFromSilence` messages to self instead of running closures.
 *  - Lifecycle hooks are `Behaviors.setup` (the constructor) and the `PostStop`
 *    signal.
 */
class Neuron(context: ActorContext[Command],
                  timers: TimerScheduler[Command],
                  val id: String,
                  val netId: String,
                  val threshold: Double,
                  val silenceIterations: SilenceIterations,
                  protected var synapses: List[Synapse])
  extends AbstractBehavior[Command](context) with NeuronTriggers {

  require(silenceIterations >= SilenceForever)
  require(threshold >= 0.0 && threshold <= 1.0)

  protected var buffer = 0.0
  private var sleeping = false

  private def printBuffer = Utils.round(buffer)

  // `LOG += str` in the classic Neuron relied on an implicit `Neuron` (`this`).
  // Neuron isn't a `Neuron`, so we log through the id-prefixed helper instead.
  protected def log(str: String): Unit = LOG.debug(s"$id: $str")

  private lazy val silentForever = silenceIterations == SilenceForever

  // --- active state -------------------------------------------------------

  override def onMessage(msg: Command): Behavior[Command] = msg match {
    case Signal(s, senderId) =>
      log(s"signal $s from $senderId received")
      this += s
      this
    case SilenceRequest =>
      log("silence request received")
      becomeSilent()
    case WakeUp =>
      log("waking up")
      sleeping = false
      tick()
      this
    case Reset(replyTo) =>
      reset(replyTo)
      this
    case other =>
      handleCommon(other)
      this
  }

  // --- silent state -------------------------------------------------------

  private val silentBehavior: Behavior[Command] = Behaviors.receiveMessage {
    case Signal(s, senderId) =>
      log(s"signal $s from $senderId ignored")
      triggerSignalIgnored()
      Behaviors.same
    case WakeRequest =>
      log("wake request received")
      wakeFromSilence()
    case WakeFromSilence =>
      wakeFromSilence()
    case Reset(replyTo) =>
      reset(replyTo)
      this // back to the active behaviour
    case other =>
      handleCommon(other)
      Behaviors.same
  }

  // --- common messages (queries & triggers) -------------------------------

  protected def handleCommon(msg: Command): Unit = msg match {
    case GetId(replyTo)               => replyTo ! Msg(0.0, id)
    case GetData(replyTo)             => replyTo ! info
    case FindSynapse(destId, replyTo) => replyTo ! SynapseOpt(findSynapse(destId))
    case GetSynapses(replyTo)         => replyTo ! Synapses(synapses)
    case SetSynapses(s)               => this.synapses = s.toList
    case AddAfterFireTrigger(tid, f, replyTo) =>
      addAfterFire(tid, f); replyTo ! Success(tid)
    case RemoveAfterFireTrigger(tid, replyTo) =>
      removeAfterFire(tid); replyTo ! Success(tid)
    case AddSilenceRequestedTrigger(tid, f, replyTo) =>
      addSilenceRequested(tid, f); replyTo ! Success(tid)
    case RemoveSilenceRequestedTrigger(tid, replyTo) =>
      removeSilenceRequested(tid); replyTo ! Success(tid)
    case AddSignalIgnoredTrigger(tid, f, replyTo) =>
      addSignalIgnored(tid, f); replyTo ! Success(tid)
    case RemoveSignalIgnoredTrigger(tid, replyTo) =>
      removeSignalIgnored(tid); replyTo ! Success(tid)
    case RemoveAllTriggers(replyTo) =>
      removeAllTriggers(); replyTo ! Success(id)
    case other =>
      log(s"$id, unrecognized message: $other")
  }

  // --- behaviour transitions & internals ----------------------------------

  private def becomeSilent(): Behavior[Command] = {
    log(s"becomeSilent, silenceIterations is $silenceIterations")
    buffer = 0.0

    if (sleeping) {
      timers.cancel(WakeUp)
      sleeping = false
    }

    val next: Behavior[Command] =
      if (silentForever) {
        log("becoming silent forever")
        silentBehavior
      } else {
        val t = Context().iterationTime * silenceIterations
        if (t > 0) {
          timers.startSingleTimer(WakeFromSilence, t.millis)
          silentBehavior
        } else this
      }

    triggerSilenceRequested()
    next
  }

  private def wakeFromSilence(): Behavior[Command] = {
    log("waking up from a silence request")
    this
  }

  private def makeSleep(): Unit = {
    log(s"going to sleep for ${Context().iterationTime} ms")
    sleeping = true
    timers.startSingleTimer(WakeUp, Context().iterationTime.millis)
  }

  protected def +=(signal: Double): Unit = {
    log(s"adding signal $signal to buffer $printBuffer, threshold is $threshold")
    buffer += signal
    log(s"(after adding) buffer is now $printBuffer")
    if (!sleeping) tick()
  }

  private def biggerOrCloseEnough(x: Double, y: Double) = x > 0.0 && x + 0.001 > y

  private def tick(): Unit = this.synchronized {
    buffer = Utils.minmax(-1.0, buffer, 1.0)

    if (biggerOrCloseEnough(buffer, threshold)) {
      buffer = 0.0
      log("Fire!")
      makeSleep()
      synapses.foreach(_.send(1.0, id))
      triggerAfterFire()
    }

    log(s"(tick) buffer is now $printBuffer")
  }

  protected def reset(replyTo: ActorRef[Answer]): Unit = {
    buffer = 0.0
    log(s"(reset) buffer is now $printBuffer")
    timers.cancelAll()
    sleeping = false
    replyTo ! Success(id)
  }

  private def findSynapse(destinationId: String): Option[Synapse] =
    if (synapses.nonEmpty) synapses.find(_.dest.id == destinationId) else None

  def info: NeuronInfo =
    NeuronInfo(id, netId, threshold, silenceIterations, synapses.map(_.info), buffer)

  // --- lifecycle ----------------------------------------------------------

  override def onSignal: PartialFunction[AkkaSignal, Behavior[Command]] = {
    case PostStop =>
      NeuronCounter.unreg(netId, id)
      this
  }
}

object Neuron {
  type SilenceIterations = Int
  val SilenceForever: SilenceIterations = -1

  /** The message protocol understood by a [[Neuron]] actor. */
  sealed trait Command

  // signals
  final case class Signal(s: Double, senderId: String) extends Command
  case object SilenceRequest extends Command
  case object WakeRequest extends Command

  // internal self-messages (replace the scheduled closures of the classic version)
  private[async] case object WakeUp extends Command
  private[async] case object WakeFromSilence extends Command

  // queries (carry an explicit replyTo instead of relying on sender())
  final case class GetId(replyTo: ActorRef[Msg]) extends Command
  final case class GetData(replyTo: ActorRef[NeuronInfo]) extends Command
  final case class FindSynapse(destinationId: String, replyTo: ActorRef[SynapseOpt]) extends Command
  final case class GetSynapses(replyTo: ActorRef[Synapses]) extends Command

  // synapse replies (typed equivalents of Messages.MsgSynapses / MsgSynapse)
  final case class Synapses(synapses: Seq[Synapse])
  final case class SynapseOpt(synapse: Option[Synapse])

  // commands
  final case class SetSynapses(synapses: Seq[Synapse]) extends Command
  final case class RemoveAllTriggers(replyTo: ActorRef[Answer]) extends Command
  final case class Reset(replyTo: ActorRef[Answer]) extends Command

  // triggers
  final case class AddAfterFireTrigger(id: String, f: Trigger, replyTo: ActorRef[Answer]) extends Command
  final case class RemoveAfterFireTrigger(id: String, replyTo: ActorRef[Answer]) extends Command
  final case class AddSilenceRequestedTrigger(id: String, f: Trigger, replyTo: ActorRef[Answer]) extends Command
  final case class RemoveSilenceRequestedTrigger(id: String, replyTo: ActorRef[Answer]) extends Command
  final case class AddSignalIgnoredTrigger(id: String, f: Trigger, replyTo: ActorRef[Answer]) extends Command
  final case class RemoveSignalIgnoredTrigger(id: String, replyTo: ActorRef[Answer]) extends Command

  /** Factory for the initial (active) behaviour of a neuron. */
  def apply(id: String,
            netId: String,
            threshold: Double,
            silenceIterations: SilenceIterations,
            synapses: List[Synapse] = List.empty): Behavior[Command] =
    Behaviors.setup { context =>
      Behaviors.withTimers { timers =>
        NeuronCounter.reg(netId, id, context.self.toClassic)
        new Neuron(context, timers, id, netId, threshold, silenceIterations, synapses)
      }
    }
}
