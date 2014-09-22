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
  def !(any: Any) = {
    debug(this, "received: " + any.toString)
    ref ! any
  } 
  def ?(any: Any) = ref ? any
  
  def inputIds = await[MsgNeurons](ref,GetInputLayer).neurons.map( _.id )
  def inputSize = await[MsgNeurons](ref,GetInputLayer).neurons.size
  def middleIds = await[MsgNeurons](ref,GetMiddleLayer).neurons.map( _.id )
  def middleSize = await[MsgNeurons](ref,GetMiddleLayer).neurons.size
  def outputIds = await[MsgNeurons](ref,GetOutputLayer).neurons.map( _.id )
  def outputSize = await[MsgNeurons](ref,GetOutputLayer).neurons.size

  def find(id: String) = await[MsgNeuron](ref, GetNeuron(id))

  def createNeuron(id: String, treshold: Double, slope: Double, forgetting: Double) = 
    await[NeuronRef](ref, CreateNeuron(id, treshold, slope, forgetting))
 
  def connectNeurons(id1: String, id2: String, weight: Double) = await[Answer](ref, ConnectNeurons(id1, id2, weight))
  
  def init() = await[Answer](ref, Init) match {
    case Success(netId) if(netId == "netinit_"+id) => debug(this,"initialization succeeded " + netId); true
    case Failure(msg) => error(this, msg); false
  }
  
  def setInputLayer(seq: Seq[String]) = await[Answer](ref, SetInputLayer(seq))
  def setOutputLayer(seq: Seq[String]) = await[Answer](ref, SetOutputLayer(seq))
}

object NetRef {
  private val netRefMap = mutable.HashMap[String,NetRef]()
  
  def apply(id: String):NetRef = netRefMap.getOrElseUpdate(id, 
    new NetRef(id, system.actorOf(Props(new AkkaNet(id))))
  )
   
  def apply(id: String, defSlope: Double, defTreshold: Double, defWeight: Double):NetRef = netRefMap.get(id) match {
    case Some(netref) => throw new IllegalArgumentException(s"The net $id cannot be created - already exists")
    case None => {
      val ref = system.actorOf(Props(new AkkaNet(id, defSlope, defTreshold, defWeight)))
      val netref = new NetRef(id, ref)
      netRefMap += ((id, netref))
      netref
    }
  }
}