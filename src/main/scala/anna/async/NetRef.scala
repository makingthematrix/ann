package anna.async

import akka.actor.{ActorRef, Props}
import anna.Context
import anna.async.Messages._
import anna.data.NeuronData
import anna.logger.LOG._
import anna.utils.Utils.await

class NetRef(val id: String, val ref: ActorRef) {
  implicit val timeout = Context().timeout

  private var _iteration = 0L
  
  def iteration = _iteration
  
  def !(any: Any) = ref ! any
  
  def inputIds = inputs.map( _.id )
  def inputSize = inputs.size
  def inputs = await[MsgNeurons](ref,GetInputs).neurons

  def neurons = await[MsgNeurons](ref,GetNeurons).neurons
  def neuronsSize = neurons.size
  def neuronsIds = neurons.map( _.id )

  def find(id: String) = await[MsgNeuron](ref, GetNeuron(id))

  def createNeuron(id: String, threshold: Double, silenceIterations: Int) =
    await[NeuronRef](ref, CreateNeuron(NeuronData(id, threshold, silenceIterations, Nil)))

  def createDummy(id: String, silenceIterations: Int) =
    await[NeuronRef](ref, CreateNeuron(NeuronData(id, silenceIterations)))

  def createSilencingNeuron(id: String) = await[NeuronRef](ref, CreateNeuron(NeuronData(id)))

  def setInputs(seq: List[String]) = await[Answer](ref, SetInputs(seq))

  def signal(seq: List[Double]) = {
    ref ! SignalList(seq)
    _iteration += 1
  }

  def shutdown() = await[NetShutdownDone](ref, Shutdown)

  def info(id: String):NeuronInfo = find(id).neuronOpt match {
    case Some(neuronRef) => neuronRef.info
    case None => throw new IllegalArgumentException(s"Unable to find neuron with id $id")
  }

  def addAfterFireToAll(name: String) (f: => Any) = neurons.foreach(_.addAfterFire(name)(f))
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

  def reset() = await[Success](ref,Reset)
  def removeAllTriggers() = await[Success](ref, RemoveAllTriggers)

  def removeAfterFire(id:String) = await[Success](ref, RemoveAfterFireTrigger(id))
  def removeAfterFireFromAll(name: String) = neurons.foreach(_.removeAfterFire(name))
  def removeSilenceRequested(id:String) = await[Success](ref, RemoveSilenceRequestedTrigger(id))
  def removeSilenceRequestedFromAll(name: String) = neurons.foreach(_.removeSilenceRequested(name))
  def removeSignalIgnored(id:String) = await[Success](ref, RemoveSignalIgnoredTrigger(id))
  def removeSignalIgnoredFromAll(name: String) = neurons.foreach(_.removeSignalIgnored(name))
}

object NetRef {
  private var netRefOpt: Option[NetRef] = None
  
  def apply(id: String):NetRef = {
    val ref = Context().system.actorOf(Props(new Net(id)))
    val netRef = new NetRef(id, ref)
    netRefOpt = Some(netRef)
    netRef
  }
  
  def get = netRefOpt
}