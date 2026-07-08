package anna.async

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef, Scheduler}
import akka.util.Timeout
import anna.Context
import anna.async.Messages.{Answer, NetShutdownDone, NeuronInfo}
import anna.async.NetTyped._
import anna.data.NeuronData
import anna.logger.LOG._

import scala.concurrent.Await

/**
 * Typed-Actor version of [[NetRef]] — an "outside the actor" handle to a
 * [[NetTyped]] instance.
 *
 * Like [[NeuronRefTyped]] it uses `akka.actor.typed.scaladsl.AskPattern` and
 * blocks on the returned `Future` to preserve the classic synchronous API.
 * Neuron handles returned by the net are wrapped as [[NeuronRefTyped]].
 */
final class NetRefTyped(val id: String, val ref: ActorRef[NetTyped.Command]) {

  private var _iteration = 0L
  def iteration: Long = _iteration

  private def ask[Res](f: ActorRef[Res] => NetTyped.Command): Res = {
    implicit val timeout: Timeout = Context().timeout
    implicit val scheduler: Scheduler = Context().system.toTyped.scheduler
    Await.result(ref.ask(f), timeout.duration)
  }

  private def toRef(h: NeuronHandle): NeuronRefTyped = NeuronRefTyped(h.id, h.ref)

  def !(msg: NetTyped.Command): Unit = ref ! msg

  def inputs: Seq[NeuronRefTyped] = ask[Neurons](GetInputs(_)).neurons.map(toRef)
  def inputIds: Seq[String] = inputs.map(_.id)
  def inputSize: Int = inputs.size

  def neurons: Seq[NeuronRefTyped] = ask[Neurons](GetNeurons(_)).neurons.map(toRef)
  def neuronsSize: Int = neurons.size
  def neuronsIds: Seq[String] = neurons.map(_.id)

  def find(id: String): Option[NeuronRefTyped] = ask[NeuronOpt](GetNeuron(id, _)).neuron.map(toRef)

  def createNeuron(id: String, threshold: Double, silenceIterations: Int): NeuronRefTyped =
    toRef(ask[NeuronHandle](CreateNeuron(NeuronData(id, threshold, silenceIterations, Nil), _)))

  def createDummy(id: String, silenceIterations: Int): NeuronRefTyped =
    toRef(ask[NeuronHandle](CreateNeuron(NeuronData(id, silenceIterations), _)))

  def createSilencingNeuron(id: String): NeuronRefTyped =
    toRef(ask[NeuronHandle](CreateNeuron(NeuronData(id), _)))

  def setInputs(seq: List[String]): Answer = ask[Answer](SetInputs(seq, _))

  def signal(seq: List[Double]): Unit = {
    ref ! SignalList(seq)
    _iteration += 1
  }

  def shutdown(): NetShutdownDone = ask[NetShutdownDone](Shutdown(_))

  def info(id: String): NeuronInfo = find(id) match {
    case Some(neuronRef) => neuronRef.info
    case None => throw new IllegalArgumentException(s"Unable to find neuron with id $id")
  }

  def addAfterFireToAll(name: String)(f: => Any): Unit = neurons.foreach(_.addAfterFire(name)(f))
  def addAfterFire(id: String, name: String)(f: => Any): Unit = find(id) match {
    case Some(neuronRef) => neuronRef.addAfterFire(name)(f)
    case None => error(this, s"Unable to find neuron with id $id")
  }
  def addAfterFire(id: String)(f: => Any): Unit = addAfterFire(id, id)(f)

  def addSilenceRequested(id: String, name: String)(f: => Any): Unit = find(id) match {
    case Some(neuronRef) => neuronRef.addSilenceRequested(name)(f)
    case None => error(this, s"Unable to find neuron with id $id")
  }
  def addSilenceRequested(id: String)(f: => Any): Unit = addSilenceRequested(id, id)(f)

  def addSignalIgnored(id: String, name: String)(f: => Any): Unit = find(id) match {
    case Some(neuronRef) => neuronRef.addSignalIgnored(name)(f)
    case None => error(this, s"Unable to find neuron with id $id")
  }
  def addSignalIgnored(id: String)(f: => Any): Unit = addSignalIgnored(id, id)(f)

  def reset(): Answer = ask[Answer](Reset(_))
  def removeAllTriggers(): Answer = ask[Answer](RemoveAllTriggers(_))
  def removeAfterFire(id: String): Answer = ask[Answer](RemoveAfterFireTrigger(id, _))

  def removeAfterFireFromAll(name: String): Unit = neurons.foreach(_.removeAfterFire(name))
  def removeSilenceRequestedFromAll(name: String): Unit = neurons.foreach(_.removeSilenceRequested(name))
  def removeSignalIgnoredFromAll(name: String): Unit = neurons.foreach(_.removeSignalIgnored(name))
}

object NetRefTyped {
  def apply(id: String): NetRefTyped = {
    // Uses the classic-system adapter to spawn a top-level typed actor without a
    // dedicated guardian; a real typed ActorSystem/guardian is a later step.
    val ref = Context().system.spawnAnonymous(NetTyped(id))
    new NetRefTyped(id, ref)
  }

  def apply(id: String, ref: ActorRef[NetTyped.Command]): NetRefTyped = new NetRefTyped(id, ref)
}
