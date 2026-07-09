package anna.async

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, PostStop, Signal => AkkaSignal}
import anna.async.Messages.{Answer, Failure, Msg, NetShutdownDone, Success}
import anna.async.NetTyped._
import anna.data.NeuronData

import scala.collection.mutable

/**
 * Typed-Actor version of [[Net]].
 *
 * A straight port of the classic (untyped) `Net` to `akka.actor.typed`,
 * following the same object-oriented `AbstractBehavior` style used for
 * [[NeuronTyped]]:
 *
 *  - Explicit sealed `Command` protocol; every request that relied on
 *    `sender()` now carries an explicit `replyTo`.
 *  - Child neurons are spawned as [[NeuronTyped]] (standard/dummy) and
 *    [[SilencingNeuronTyped]] (silencing) via `context.spawn`, and are held in
 *    an ordered map instead of being looked up through the `ActorContext`
 *    (as recommended by the migration guide).
 *  - `context.watch` + the classic `Terminated` message become
 *    `context.watchWith(ref, ChildTerminated(id))`.
 *  - The `waiting` hot-swap state (broadcast a command to every neuron, then
 *    reply to the caller once all acked) is a returned `Behavior`. Since child
 *    replies are of type `Answer` (not `Command`), a message adapter wraps them
 *    in `WrappedAnswer`. The wait is counted rather than id-matched, which also
 *    fixes the latent bug in the classic `removeAfterFire` (whose per-neuron
 *    `Success` carried the trigger id, not the neuron id).
 *  - `PoisonPill` shutdown becomes `context.stop` of the children and
 *    `Behaviors.stopped` once the last child terminates; the caller is notified
 *    from the `PostStop` signal.
 *
 * The original [[Net]] is left untouched so both can coexist.
 */
class NetTyped(context: ActorContext[Command], val id: String)
  extends AbstractBehavior[Command](context) {

  private val neurons = mutable.LinkedHashMap[String, ActorRef[NeuronTyped.Command]]()
  private var inputs: List[NeuronHandle] = List.empty
  private var shutdownCallerOpt: Option[ActorRef[NetShutdownDone]] = None

  // Children reply with `Answer` (not `Command`); adapt those replies into the
  // net's own protocol so the `waiting` state can observe them.
  private val answerAdapter: ActorRef[Answer] = context.messageAdapter[Answer](WrappedAnswer.apply)

  // --- active state -------------------------------------------------------

  override def onMessage(msg: Command): Behavior[Command] = msg match {
    case GetId(replyTo)          => replyTo ! Msg(0.0, id); this
    case GetNeurons(replyTo)     => replyTo ! Neurons(neuronHandles); this
    case GetInputs(replyTo)      => replyTo ! Neurons(inputs); this
    case GetNeuron(nid, replyTo) => replyTo ! NeuronOpt(findHandle(nid)); this
    case CreateNeuron(data, replyTo) => createNeuron(data, replyTo); this
    case SetInputs(ids, replyTo) => setInputs(ids.toSet, replyTo); this
    case SignalList(in)          => signal(in); this

    case Reset(replyTo) =>
      broadcastAndWait(replyTo)(_ ! NeuronTyped.Reset(answerAdapter))
    case RemoveAllTriggers(replyTo) =>
      broadcastAndWait(replyTo)(_ ! NeuronTyped.RemoveAllTriggers(answerAdapter))
    case RemoveAfterFireTrigger(tid, replyTo) =>
      broadcastAndWait(replyTo)(_ ! NeuronTyped.RemoveAfterFireTrigger(tid, answerAdapter))

    case Shutdown(replyTo) => shutdown(replyTo)

    case ChildTerminated(nid) =>
      neurons -= nid
      if (neurons.isEmpty) Behaviors.stopped else this

    case _: WrappedAnswer => this // unexpected outside the waiting state
  }

  // --- waiting (hot-swap) state -------------------------------------------

  private def broadcastAndWait(caller: ActorRef[Answer])
                              (send: ActorRef[NeuronTyped.Command] => Unit): Behavior[Command] = {
    neurons.values.foreach(send)
    if (neurons.isEmpty) { caller ! Success(id); this }
    else waiting(caller, neurons.size)
  }

  private def waiting(caller: ActorRef[Answer], remaining: Int): Behavior[Command] =
    Behaviors.receiveMessage {
      case WrappedAnswer(Success(_)) =>
        val left = remaining - 1
        if (left <= 0) { caller ! Success(id); this } else waiting(caller, left)
      case _: WrappedAnswer =>
        Behaviors.same
      case ChildTerminated(nid) =>
        neurons -= nid
        Behaviors.same
      case _ =>
        // Other messages are ignored while waiting, as in the classic Net.
        Behaviors.same
    }

  // --- shutdown -----------------------------------------------------------

  private def shutdown(replyTo: ActorRef[NetShutdownDone]): Behavior[Command] = {
    shutdownCallerOpt = Some(replyTo)
    if (neurons.nonEmpty) {
      neurons.values.foreach(context.stop)
      this // the ChildTerminated handler stops the net once the last child is gone
    } else Behaviors.stopped
  }

  // --- neuron creation ----------------------------------------------------

  private def createNeuron(data: NeuronData, replyTo: ActorRef[NeuronHandle]): Unit = {
    val behavior: Behavior[NeuronTyped.Command] = data.neuronType match {
      case NeuronTypeStandard()  => NeuronTyped(data.id, id, data.threshold, data.silenceIterations)
      case NeuronTypeDummy()     => NeuronTyped(data.id, id, 0.0, 0)
      case NeuronTypeSilencing() => SilencingNeuronTyped(data.id, id)
    }
    // Spawn anonymously: neuron ids are bookkeeping only and may contain
    // characters that are illegal in Akka actor names (e.g. '#' from block
    // naming). The classic Net likewise created anonymous actors.
    val ref = context.spawnAnonymous(behavior)
    neurons += (data.id -> ref)
    context.watchWith(ref, ChildTerminated(data.id))
    replyTo ! NeuronHandle(data.id, ref)
  }

  // --- inputs & signalling ------------------------------------------------

  private def setInputs(ids: Set[String], replyTo: ActorRef[Answer]): Unit = {
    inputs = neuronHandles.filter(h => ids.contains(h.id)).sortBy(_.id)
    if (inputs.size != ids.size) {
      val inIds = inputs.map(_.id)
      val notFound = ids.filterNot(inIds.contains)
      replyTo ! Failure(s"setInputs, unable to find neurons with ids: $notFound")
    } else {
      replyTo ! Success("setInputLayer_" + id)
    }
  }

  private def signal(in: List[Double]): Unit = {
    assert(in.size == inputs.size,
      s"Difference in size between the input layer (${inputs.size}) and the input (${in.size})")
    inputs.zip(in).foreach { case (handle, s) => handle.ref ! NeuronTyped.Signal(s, "Input") }
  }

  // --- helpers ------------------------------------------------------------

  private def neuronHandles: List[NeuronHandle] =
    neurons.iterator.map { case (nid, ref) => NeuronHandle(nid, ref) }.toList

  private def findHandle(nid: String): Option[NeuronHandle] =
    neurons.get(nid).map(NeuronHandle(nid, _))

  // --- lifecycle ----------------------------------------------------------

  override def onSignal: PartialFunction[AkkaSignal, Behavior[Command]] = {
    case PostStop =>
      shutdownCallerOpt.foreach(_ ! NetShutdownDone(id))
      this
  }
}

object NetTyped {
  /** A typed reference to a spawned neuron, together with its id. */
  final case class NeuronHandle(id: String, ref: ActorRef[NeuronTyped.Command])

  /** The message protocol understood by a [[NetTyped]] actor. */
  sealed trait Command

  // queries (carry an explicit replyTo instead of relying on sender())
  final case class GetId(replyTo: ActorRef[Msg]) extends Command
  final case class GetNeurons(replyTo: ActorRef[Neurons]) extends Command
  final case class GetInputs(replyTo: ActorRef[Neurons]) extends Command
  final case class GetNeuron(id: String, replyTo: ActorRef[NeuronOpt]) extends Command

  // commands
  final case class CreateNeuron(data: NeuronData, replyTo: ActorRef[NeuronHandle]) extends Command
  final case class SetInputs(ids: Seq[String], replyTo: ActorRef[Answer]) extends Command
  final case class SignalList(inputs: List[Double]) extends Command
  final case class Reset(replyTo: ActorRef[Answer]) extends Command
  final case class RemoveAllTriggers(replyTo: ActorRef[Answer]) extends Command
  final case class RemoveAfterFireTrigger(id: String, replyTo: ActorRef[Answer]) extends Command
  final case class Shutdown(replyTo: ActorRef[NetShutdownDone]) extends Command

  // internal self-messages
  private[async] final case class WrappedAnswer(answer: Answer) extends Command
  private[async] final case class ChildTerminated(id: String) extends Command

  // replies
  final case class Neurons(neurons: Seq[NeuronHandle])
  final case class NeuronOpt(neuron: Option[NeuronHandle])

  def apply(id: String): Behavior[Command] =
    Behaviors.setup(context => new NetTyped(context, id))
}
