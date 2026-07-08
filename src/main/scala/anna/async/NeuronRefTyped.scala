package anna.async

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef, Scheduler}
import akka.util.Timeout
import anna.Context
import anna.async.Messages.{Answer, Failure, MsgSynapses, NeuronInfo, Success}
import anna.logger.LOG._

import scala.concurrent.Await

/**
 * Typed-Actor version of [[NeuronRef]] — an "outside the actor" handle to a
 * [[NeuronTyped]] instance.
 *
 * Where the classic `NeuronRef` used the classic `ask` (`?`) via
 * `Utils.await`, this version uses `akka.actor.typed.scaladsl.AskPattern`. The
 * synchronous ergonomics of the original are kept: each request-response call
 * still blocks on the returned `Future` with the configured timeout, so the
 * public API stays drop-in comparable.
 */
final class NeuronRefTyped(val id: String, val ref: ActorRef[NeuronTyped.Command]) {

  private def ask[Res](f: ActorRef[Res] => NeuronTyped.Command): Res = {
    implicit val timeout: Timeout = Context().timeout
    implicit val scheduler: Scheduler = Context().system.toTyped.scheduler
    Await.result(ref.ask(f), timeout.duration)
  }

  private def triggerResult(answer: Answer, op: String): Boolean = answer match {
    case Success(_)   => true
    case Failure(str) => error(this, s"$op failure: $str"); false
    case other        => error(this, s"Wrong answer: $other"); false
  }

  def info: NeuronInfo = ask[NeuronInfo](NeuronTyped.GetData(_))
  def getSynapses: Seq[Synapse] = ask[MsgSynapses](NeuronTyped.GetSynapses(_)).synapses
  def setSynapses(synapses: Seq[Synapse]): Unit =
    if (synapses.nonEmpty) ref ! NeuronTyped.SetSynapses(synapses)

  def requestSilence(): Unit = {
    debug(this, s"request silence for $id")
    ref ! NeuronTyped.SilenceRequest
  }

  def requestWakeUp(): Unit = {
    debug(this, s"request wake up for $id")
    ref ! NeuronTyped.WakeRequest
  }

  protected def calculateOutput: Double = Double.NaN // we don't do that here

  def addAfterFire(triggerId: String)(f: => Any): Boolean =
    triggerResult(ask[Answer](NeuronTyped.AddAfterFireTrigger(triggerId, () => f, _)), "addAfterFire")
  def removeAfterFire(name: String): Boolean =
    triggerResult(ask[Answer](NeuronTyped.RemoveAfterFireTrigger(name, _)), "removeAfterFire")

  def addSilenceRequested(triggerId: String)(f: => Any): Boolean =
    triggerResult(ask[Answer](NeuronTyped.AddSilenceRequestedTrigger(triggerId, () => f, _)), "addSilenceRequested")
  def removeSilenceRequested(name: String): Boolean =
    triggerResult(ask[Answer](NeuronTyped.RemoveSilenceRequestedTrigger(name, _)), "removeSilenceRequested")

  def addSignalIgnored(triggerId: String)(f: => Any): Boolean =
    triggerResult(ask[Answer](NeuronTyped.AddSignalIgnoredTrigger(triggerId, () => f, _)), "addSignalIgnored")
  def removeSignalIgnored(name: String): Boolean =
    triggerResult(ask[Answer](NeuronTyped.RemoveSignalIgnoredTrigger(name, _)), "removeSignalIgnored")

  def removeAllTriggers(): Boolean =
    triggerResult(ask[Answer](NeuronTyped.RemoveAllTriggers(_)), "removeAllTriggers")
  def reset(): Boolean =
    triggerResult(ask[Answer](NeuronTyped.Reset(_)), "reset")

  def +=(signal: Double): Unit = ref ! NeuronTyped.Signal(signal, id)
  def !(msg: NeuronTyped.Command): Unit = ref ! msg
}

object NeuronRefTyped {
  def apply(id: String, ref: ActorRef[NeuronTyped.Command]): NeuronRefTyped = new NeuronRefTyped(id, ref)
}
