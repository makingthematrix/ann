package anna.async

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, Scheduler}
import akka.util.Timeout
import anna.Context
import anna.async.Messages.{Answer, Failure, NeuronInfo, Success}
import anna.logger.LOG._

import scala.concurrent.Await

/**
 * An "outside the actor" handle to a [[Neuron]] instance.
 *
 * It uses `akka.actor.typed.scaladsl.AskPattern` for request-response and
 * blocks on the returned `Future` with the configured timeout, exposing a
 * synchronous API.
 */
final class NeuronRef(val id: String, val ref: ActorRef[Neuron.Command]) {
  private def ask[Res](f: ActorRef[Res] => Neuron.Command): Res = {
    given timeout: Timeout = Context().timeout
    given Scheduler = Context().system.scheduler
    Await.result(ref.ask(f), timeout.duration)
  }

  private def triggerResult(answer: Answer, op: String): Boolean = answer match {
    case Success(_)   => true
    case Failure(str) => error(this, s"$op failure: $str"); false
    case other        => error(this, s"Wrong answer: $other"); false
  }

  def info: NeuronInfo = ask[NeuronInfo](Neuron.GetData(_))
  def getSynapses: Seq[Synapse] = ask[Neuron.Synapses](Neuron.GetSynapses(_)).synapses
  def setSynapses(synapses: Seq[Synapse]): Unit =
    if (synapses.nonEmpty) ref ! Neuron.SetSynapses(synapses)

  def requestSilence(): Unit = {
    debug(this, s"request silence for $id")
    ref ! Neuron.SilenceRequest
  }

  def requestWakeUp(): Unit = {
    debug(this, s"request wake up for $id")
    ref ! Neuron.WakeRequest
  }

  protected def calculateOutput: Double = Double.NaN // we don't do that here

  def addAfterFire(triggerId: String)(f: => Any): Boolean =
    triggerResult(ask[Answer](Neuron.AddAfterFireTrigger(triggerId, () => f, _)), "addAfterFire")
  def removeAfterFire(name: String): Boolean =
    triggerResult(ask[Answer](Neuron.RemoveAfterFireTrigger(name, _)), "removeAfterFire")

  def addSilenceRequested(triggerId: String)(f: => Any): Boolean =
    triggerResult(ask[Answer](Neuron.AddSilenceRequestedTrigger(triggerId, () => f, _)), "addSilenceRequested")
  def removeSilenceRequested(name: String): Boolean =
    triggerResult(ask[Answer](Neuron.RemoveSilenceRequestedTrigger(name, _)), "removeSilenceRequested")

  def addSignalIgnored(triggerId: String)(f: => Any): Boolean =
    triggerResult(ask[Answer](Neuron.AddSignalIgnoredTrigger(triggerId, () => f, _)), "addSignalIgnored")
  def removeSignalIgnored(name: String): Boolean =
    triggerResult(ask[Answer](Neuron.RemoveSignalIgnoredTrigger(name, _)), "removeSignalIgnored")

  def removeAllTriggers(): Boolean =
    triggerResult(ask[Answer](Neuron.RemoveAllTriggers(_)), "removeAllTriggers")
  def reset(): Boolean =
    triggerResult(ask[Answer](Neuron.Reset(_)), "reset")

  def +=(signal: Double): Unit = ref ! Neuron.Signal(signal, id)
  def !(msg: Neuron.Command): Unit = ref ! msg
}

object NeuronRef {
  def apply(id: String, ref: ActorRef[Neuron.Command]): NeuronRef = new NeuronRef(id, ref)
}
