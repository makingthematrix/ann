package main.async

import akka.actor.ActorRef
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.pattern.ask
import Context._
import main.logger.LOG._
import main.utils.Utils.await
import Messages._

class NeuronRef(val id: String, val ref: ActorRef) {
  def input = await[Msg](ref, GetInput).d
  def lastOutput = await[Msg](ref, GetLastOutput).d
  def getSynapses = await[MsgSynapses](ref, GetSynapses).synapses
  
  def silence() = ref ! HushNow
  
  def connect(dest: NeuronRef, weight: Double) = await[Answer](ref, Connect(dest, weight)) match {
    case Success(id) => true
    case Failure(str) => println(s"NeuronRef.connect failure: $str"); false 
  }
  
  protected def calculateOutput = Double.NaN // we don't do that here 
  
  def addAfterFireTrigger(name: String, f:(Neuron) => Any) = ref ! AddAfterFireTrigger(name, f)
  
  def +=(signal: Double) = ref ! Signal(signal) 
  
  def !(any: Any) = {
    debug(this,s"$id, received: ${any.toString}")
    ref ! any
  }

  def ?(any: Any) = ref ? any
} 

object NeuronRef {
  def apply(id: String):NeuronRef = {
    val ref = system.actorOf(Props(new Neuron(id)), name=id)
    new NeuronRef(id, ref)
  }
  
  def apply(id: String, treshold: Double, slope: Double, forgetting: ForgettingTick):NeuronRef = {
    debug(this,s"new neuronref $id with treshold $treshold and slope $slope")
    val ref = system.actorOf(Props(new Neuron(id, treshold, slope, forgetting)), name=id)
    new NeuronRef(id, ref)
  }
}