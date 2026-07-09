package anna.async

import akka.actor.{ActorRef, Props}
import akka.actor.typed.Scheduler
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout

import scala.concurrent.Await
import anna.Context
import anna.async.Messages._
import anna.data.NeuronData
import anna.logger.LOG._
import anna.utils.Utils.await

final class NetRef(val id: String, val ref: ActorRef) {
  implicit val timeout: Timeout = Context().timeout

  private var _iteration = 0L
  
  def iteration: Long = _iteration
  
  def !(any: Any): Unit = ref ! any
  
  def inputIds: Seq[String] = inputs.map( _.id )
  def inputSize: Int = inputs.size
  def inputs: Seq[NeuronRef] = await[MsgNeurons](ref,GetInputs).neurons

  def neurons: Seq[NeuronRef] = await[MsgNeurons](ref,GetNeurons).neurons
  def neuronsSize: Int = neurons.size
  def neuronsIds: Seq[String] = neurons.map( _.id )

  def find(id: String): MsgNeuron = await[MsgNeuron](ref, GetNeuron(id))

  def createNeuron(id: String, threshold: Double, silenceIterations: Int): NeuronRef =
    await[NeuronRef](ref, CreateNeuron(NeuronData(id, threshold, silenceIterations, Nil)))

  def createDummy(id: String, silenceIterations: Int): NeuronRef =
    await[NeuronRef](ref, CreateNeuron(NeuronData(id, silenceIterations)))

  def createSilencingNeuron(id: String): NeuronRef = await[NeuronRef](ref, CreateNeuron(NeuronData(id)))

  def setInputs(seq: List[String]): Answer = await[Answer](ref, SetInputs(seq))

  def signal(seq: List[Double]): Unit = {
    ref ! SignalList(seq)
    _iteration += 1
  }

  def shutdown(): NetShutdownDone = await[NetShutdownDone](ref, Shutdown)

  def info(id: String):NeuronInfo = find(id).neuronOpt match {
    case Some(neuronRef) => neuronRef.info
    case None => throw new IllegalArgumentException(s"Unable to find neuron with id $id")
  }

  def addAfterFireToAll(name: String) (f: => Any): Unit = neurons.foreach(_.addAfterFire(name)(f))
  def addAfterFire(id: String, name: String)(f: => Any):Unit = find(id).neuronOpt match {
    case Some(neuronRef) => neuronRef.addAfterFire(name)(f)
    case None => error(this,s"Unable to find neuron with id $id")
  }
  def addAfterFire(id: String)(f: => Any):Unit = addAfterFire(id, id)(f)

  def addSilenceRequested(id: String, name: String)(f: => Any):Unit = find(id).neuronOpt match {
    case Some(neuronRef) => neuronRef.addSilenceRequested(name)(f)
    case None => error(this,s"Unable to find neuron with id $id")
  }
  def addSilenceRequested(id: String)(f: => Any):Unit  = addSilenceRequested(id, id)(f)

  def addSignalIgnored(id: String, name: String)(f: => Any):Unit = find(id).neuronOpt match {
    case Some(neuronRef) => neuronRef.addSignalIgnored(name)(f)
    case None => error(this,s"Unable to find neuron with id $id")
  }
  def addSignalIgnored(id: String)(f: => Any):Unit  = addSignalIgnored(id, id)(f)

  def reset(): Success = await[Success](ref,Reset)
  def removeAllTriggers(): Success = await[Success](ref, RemoveAllTriggers)

  def removeAfterFire(id:String): Success = await[Success](ref, RemoveAfterFireTrigger(id))
  def removeAfterFireFromAll(name: String): Unit = neurons.foreach(_.removeAfterFire(name))
  def removeSilenceRequested(id:String): Success = await[Success](ref, RemoveSilenceRequestedTrigger(id))
  def removeSilenceRequestedFromAll(name: String): Unit = neurons.foreach(_.removeSilenceRequested(name))
  def removeSignalIgnored(id:String): Success = await[Success](ref, RemoveSignalIgnoredTrigger(id))
  def removeSignalIgnoredFromAll(name: String): Unit = neurons.foreach(_.removeSignalIgnored(name))
}

object NetRef {
  private var netRefOpt: Option[NetRef] = None
  
  def apply(id: String):NetRef = {
    // Context().system is a typed ActorSystem with a custom guardian, so we ask
    // the guardian to create the classic Net actor as its child.
    val system = Context().system
    implicit val timeout: Timeout = Context().timeout
    implicit val scheduler: Scheduler = system.scheduler
    val ref = Await.result(
      system.ask[ActorRef](Guardian.SpawnClassic(Props(new Net(id)), _)),
      timeout.duration
    )
    val netRef = new NetRef(id, ref)
    netRefOpt = Some(netRef)
    netRef
  }
  
  def get: Option[NetRef] = netRefOpt
}