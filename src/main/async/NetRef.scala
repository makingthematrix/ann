package main.async

import akka.actor.ActorRef
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask

class NetRef(val id: String, val ref: ActorRef) {
  implicit var timeout = Timeout(5 seconds)
  
  def !(any: Any) = ref ! any
  def ?(any: Any) = ref ? any
  
  def inputIds = Await.result(ref ? GetInputLayer, timeout.duration).asInstanceOf[MsgNeurons].neurons.map( _.id )
  def inputSize = Await.result(ref ? GetInputLayer, timeout.duration).asInstanceOf[MsgNeurons].neurons.size
  def outputIds = Await.result(ref ? GetOutputLayer, timeout.duration).asInstanceOf[MsgNeurons].neurons.map( _.id )
  def outputSize = Await.result(ref ? GetOutputLayer, timeout.duration).asInstanceOf[MsgNeurons].neurons.size
  def find(id: String):Option[NeuronRef] = Await.result(ref ? GetNeuron(id), timeout.duration).asInstanceOf[Option[NeuronRef]]
}

object NetRef {
  val system = ActorSystem("AkkaNeuronSystem")
  
  def apply(id: String):NetRef = apply(id, system)
  
  def apply(id: String, system: ActorSystem):NetRef = {
    val ref = system.actorOf(Props(new AkkaNet(id)))
    new NetRef(id, ref)
  }
  
  def apply(id: String, defSlope: Double, defTreshold: Double, defWeight: Double, system: ActorSystem) = {
    val ref = system.actorOf(Props(new AkkaNet(id, defSlope, defTreshold, defWeight)))
    new NetRef(id, ref)
  }
}