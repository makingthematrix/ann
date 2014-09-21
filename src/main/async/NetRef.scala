package main.async

import scala.collection.mutable

import akka.actor.ActorRef
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask

import Context.system

import main.logger.LOG._

class NetRef(val id: String, val ref: ActorRef) {
  implicit var timeout = Timeout(5 seconds)
  
  def !(any: Any) = {
    debug(this, "received: " + any.toString)
    ref ! any
  } 
  def ?(any: Any) = ref ? any
  
  def inputIds = Await.result(ref ? GetInputLayer, timeout.duration).asInstanceOf[MsgNeurons].neurons.map( _.id )
  def inputSize = Await.result(ref ? GetInputLayer, timeout.duration).asInstanceOf[MsgNeurons].neurons.size
  def middleIds = Await.result(ref ? GetMiddleLayer, timeout.duration).asInstanceOf[MsgNeurons].neurons.map( _.id )
  def middleSize = Await.result(ref ? GetMiddleLayer, timeout.duration).asInstanceOf[MsgNeurons].neurons.size
  def outputIds = Await.result(ref ? GetOutputLayer, timeout.duration).asInstanceOf[MsgNeurons].neurons.map( _.id )
  def outputSize = Await.result(ref ? GetOutputLayer, timeout.duration).asInstanceOf[MsgNeurons].neurons.size

  def find(id: String):MsgNeuron = {
    debug(this, s"find $id")
    Await.result(ref ? GetNeuron(id), timeout.duration).asInstanceOf[MsgNeuron]
  }
  
  def createNeuron(id: String, treshold: Double, slope: Double, forgetting: Double) = {
    val answer = ref ? CreateNeuron(id, treshold, slope, forgetting)
    Await.result(answer, timeout.duration).asInstanceOf[NeuronRef]
  }
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