package main.async

import scala.collection.mutable

import akka.actor.ActorRef
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask

import Context._ 

import main.logger.LOG._
import Messages._
import main.utils.Utils.await

class NetRef(val id: String, val ref: ActorRef) {
  private var _iteration = 0L
  
  def iteration = _iteration
  
  def !(any: Any) = ref ! any
  def ?(any: Any) = ref ? any
  
  def inputIds = await[MsgNeurons](ref,GetInputLayer).neurons.map( _.id )
  def inputSize = await[MsgNeurons](ref,GetInputLayer).neurons.size
  def middleIds = await[MsgNeurons](ref,GetMiddleLayer).neurons.map( _.id )
  def middleSize = await[MsgNeurons](ref,GetMiddleLayer).neurons.size
  def outputIds = await[MsgNeurons](ref,GetOutputLayer).neurons.map( _.id )
  def outputSize = await[MsgNeurons](ref,GetOutputLayer).neurons.size

  def getNeurons = await[MsgNeurons](ref,GetNeurons).neurons
  
  def find(id: String) = await[MsgNeuron](ref, GetNeuron(id))

  def createNeuron(id: String, treshold: Double, slope: Double, hushValue: HushValue, forgetting: ForgettingTick) = 
    await[NeuronRef](ref, CreateNeuron(id, treshold, slope, hushValue, forgetting))
 
  def connectNeurons(id1: String, id2: String, weight: Double) = await[Answer](ref, ConnectNeurons(id1, id2, weight))
  
  def init(usePresleep: Boolean =true) = await[Answer](ref, Init(usePresleep)) match {
    case Success(netId) if(netId == "netinit_"+id) => true
    case Failure(msg) => error(this, msg); false
  }
  
  def setInputLayer(seq: Seq[String]) = await[Answer](ref, SetInputLayer(seq))
  def setOutputLayer(seq: Seq[String]) = await[Answer](ref, SetOutputLayer(seq))
  
  def signal(seq: Seq[Double]) = {
    ref ! SignalSeq(seq)
    _iteration += 1
  }
  
  def shutdown() = await[NetShutdownDone](ref,Shutdown)
 
  def addAfterFireTrigger(id: String, name: String, f: Trigger):Unit = find(id).neuronOpt match {
    case Some(neuronRef) => neuronRef.addAfterFireTrigger(name, f)
    case None => error(this,s"Unable to find neuron with id $id")
  }
  def addAfterFireTrigger(id: String, f: Trigger):Unit  = addAfterFireTrigger(id, id, f)

}

object NetRef {
  private var netRefOpt: Option[NetRef] = None
  
  def apply(id: String):NetRef = apply(id, Context.slope, Context.threshold, Context.weight)
  
  def apply(id: String, defSlope: Double, defTreshold: Double, defWeight: Double):NetRef = {
    val ref = system.actorOf(Props(new Net(id, defSlope, defTreshold, defWeight)))
    val netRef = new NetRef(id, ref)
    netRefOpt = Some(netRef)
    netRef
  }
  
  def get = netRefOpt
}