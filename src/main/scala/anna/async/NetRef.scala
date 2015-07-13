package anna.async

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import anna.Context
import anna.async.Messages._
import anna.data.{ForgetTrait, HushValue, NeuronData}
import anna.logger.LOG
import anna.logger.LOG._
import anna.utils.Utils.await

class NetRef(val id: String, val ref: ActorRef) {
  implicit val timeout = Context().timeout

  private var _iteration = 0L
  
  def iteration = _iteration
  
  def !(any: Any) = ref ! any
  
  def inputIds = await[MsgNeurons](ref,GetInputs).neurons.map( _.id )
  def inputSize = await[MsgNeurons](ref,GetInputs).neurons.size

  def getNeurons = await[MsgNeurons](ref,GetNeurons).neurons
  
  def find(id: String) = await[MsgNeuron](ref, GetNeuron(id))

  def createNeuron(
    id: String,
    threshold: Double,
    slope: Double,
    hushValue: HushValue,
    forgetting: ForgetTrait,
    tickTime: Long,
    activationFunctionName: String
  ) = await[NeuronRef](ref, CreateNeuron(NeuronData(
    id, threshold, slope, hushValue, forgetting, tickTime, activationFunctionName
  )))

  def createDummy(id: String, hushValue: HushValue, tickTime: Long) =
    await[NeuronRef](ref, CreateNeuron(NeuronData(id, hushValue, tickTime)))

  def createHushNeuron(id: String) = await[NeuronRef](ref, CreateNeuron(NeuronData(id)))
  
  def setInputs(seq: Seq[String]) = await[Answer](ref, SetInputs(seq))
  
  def signal(seq: Seq[Double]) = {
    ref ! SignalSeq(seq)
    _iteration += 1
  }
  
  def shutdown() = await[NetShutdownDone](ref,Shutdown)

  def lastOutput(id: String):Double = find(id).neuronOpt match {
    case Some(neuronRef) => neuronRef.lastOutput
    case None => throw new IllegalArgumentException(s"Unable to find neuron with id $id")
  }

  def addAfterFireToAll(name: String) (f: (Double)=> Any) = getNeurons.foreach(_.addAfterFire(name)(f))
  def addAfterFire(id: String, name: String)(f: (Double)=> Any):Unit = find(id).neuronOpt match {
    case Some(neuronRef) => neuronRef.addAfterFire(name)(f)
    case None => error(this,s"Unable to find neuron with id $id")
  }
  def addAfterFire(id: String)(f: (Double) => Any):Unit = addAfterFire(id, id)(f)

  def addHushRequested(id: String, name: String)(f: => Any):Unit = find(id).neuronOpt match {
    case Some(neuronRef) => neuronRef.addHushRequested(name)(f)
    case None => error(this,s"Unable to find neuron with id $id")
  }
  def addHushRequested(id: String)(f: => Any):Unit  = addHushRequested(id, id)(f)

  def reset() = await[Success](ref,Reset)
  def removeAllTriggers() = await[Success](ref, RemoveAllTriggers)

  def removeAfterFire(id:String) = await[Success](ref, RemoveAfterFireTrigger(id))
  def removeAfterFireFromAll(name: String) = getNeurons.foreach(_.removeAfterFire(name))
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