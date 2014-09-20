package main.async

import akka.actor.ActorRef
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.pattern.ask

import main.logger.LOG._

class NeuronRef(val id: String, val ref: ActorRef) {
  implicit var timeout = Timeout(5 seconds)
  
  def input = Await.result(ref ? GetInput, timeout.duration).asInstanceOf[Msg].d
  
  def lastOutput = Await.result(ref ? GetLastOutput, timeout.duration).asInstanceOf[Msg].d
  
  def silence() = ref ! HushNow
  
  def connect(dest: NeuronRef, weight: Double) = 
    Await.result(ref ? Connect(dest, weight), timeout.duration) match {
      case Success => true
      case Failure(str) => println(s"NeuronRef.connect failure: $str"); false 
    }
  
  protected def calculateOutput = Double.NaN // we don't do that here 
  
  def getSynapses: List[AkkaSynapse] = 
    Await.result(ref ? GetSynapses, timeout.duration).asInstanceOf[MsgSynapses].synapses
  
  def addAfterFireTrigger(name: String, f:(AkkaNeuron) => Any) = ref ! AddAfterFireTrigger(name, f)
  
  def +=(signal: Double) = ref ! Signal(signal) 
  
  def !(any: Any) = ref ! any
  def ?(any: Any) = ref ? any
} 

object NeuronRef {
  val system = ActorSystem("AkkaNeuronSystem")
  
  def apply(id: String):NeuronRef = apply(id, system)
  
  def apply(id: String, system: ActorSystem):NeuronRef = {
    val ref = system.actorOf(Props(new AkkaNeuron(id)), name=id)
    new NeuronRef(id, ref)
  }
  
  def apply(id: String, treshold: Double, slope: Double, forgetting: Double, system: ActorSystem):NeuronRef = {
    debug(this,s"new neuronref $id with treshold $treshold and slope $slope")
    val ref = system.actorOf(Props(new AkkaNeuron(id, treshold, slope, forgetting)), name=id)
    new NeuronRef(id, ref)
  }
}