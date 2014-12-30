package anna.async

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import anna.async.Context._
import anna.async.Messages._
import anna.async.logger.LOG._
import anna.data.{NeuronData, ForgetTrait, HushValue}
import anna.utils.Utils.await

class NetRef(val id: String, val ref: ActorRef) {
  private var _iteration = 0L
  
  def iteration = _iteration
  
  def !(any: Any) = ref ! any
  def ?(any: Any) = ref ? any
  
  def inputIds = await[MsgNeurons](ref,GetInputLayer).neurons.map( _.id )
  def inputSize = await[MsgNeurons](ref,GetInputLayer).neurons.size
  def middleIds = await[MsgNeurons](ref,GetMiddleLayer).neurons.map( _.id )
  def middleSize = await[MsgNeurons](ref,GetMiddleLayer).neurons.size

  def getNeurons = await[MsgNeurons](ref,GetNeurons).neurons
  
  def find(id: String) = await[MsgNeuron](ref, GetNeuron(id))

  def createNeuron(id: String, threshold: Double, slope: Double, hushValue: HushValue, forgetting: ForgetTrait, tickTime: Long) =
    await[NeuronRef](ref, CreateNeuron(NeuronData(id, threshold, slope, hushValue, forgetting, tickTime)))
  def createDummy(id: String, hushValue: HushValue, tickTime: Long) =
    await[NeuronRef](ref, CreateNeuron(NeuronData(id, hushValue, tickTime)))
  def createHushNeuron(id: String) = await[NeuronRef](ref, CreateNeuron(NeuronData(id)))
  
  def setInputLayer(seq: Seq[String]) = await[Answer](ref, SetInputLayer(seq))
  
  def signal(seq: Seq[Double]) = {
    ref ! SignalSeq(seq)
    _iteration += 1
  }
  
  def shutdown() = await[NetShutdownDone](ref,Shutdown)
 
  def addAfterFire(id: String, name: String)(f: => Any):Unit = find(id).neuronOpt match {
    case Some(neuronRef) => neuronRef.addAfterFire(name)(f)
    case None => error(this,s"Unable to find neuron with id $id")
  }
  def addAfterFire(id: String)(f: => Any):Unit  = addAfterFire(id, id)(f)

  def addHushRequested(id: String, name: String)(f: => Any):Unit = find(id).neuronOpt match {
    case Some(neuronRef) => neuronRef.addHushRequested(name)(f)
    case None => error(this,s"Unable to find neuron with id $id")
  }
  def addHushRequested(id: String)(f: => Any):Unit  = addHushRequested(id, id)(f)

}

object NetRef {
  private var netRefOpt: Option[NetRef] = None
  
  def apply(id: String):NetRef = {
    val ref = system.actorOf(Props(new Net(id)))
    val netRef = new NetRef(id, ref)
    netRefOpt = Some(netRef)
    netRef
  }
  
  def get = netRefOpt
}