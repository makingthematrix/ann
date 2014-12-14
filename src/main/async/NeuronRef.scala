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
import main.async.logger.LOG._
import main.utils.Utils.await
import Messages._

class NeuronRef(val id: String, val ref: ActorRef) {
  def input = await[Msg](ref, GetInput).d
  def lastOutput = await[Msg](ref, GetLastOutput).d
  def getSynapses = await[MsgSynapses](ref, GetSynapses).synapses
  def setSynapses(synapses: List[Synapse]) = {
    synapses.foreach( s => println(s.toString()) )
    ref ! SetSynapses(synapses)
  }
  
  def hush() = ref ! HushNow
  
  protected def calculateOutput = Double.NaN // we don't do that here 
  
  def addAfterFire(triggerId: String)(f: => Any) = await[Answer](ref, AddAfterFireTrigger(triggerId, () => f)) match {
    case Success(id) => true
    case Failure(str) => error(this,s"addAfterFire failure: $str"); false
  }
  def removeAfterFire(name: String) = await[Answer](ref, RemoveAfterFireTrigger(name)) match {
    case Success(id) => true
    case Failure(str) => error(this,s"removeAfterFire failure: $str"); false    
  }
  def addHushRequested(triggerId: String)(f: => Any) = await[Answer](ref, AddHushRequestedTrigger(triggerId, () => f)) match {
    case Success(id) => true
    case Failure(str) => error(this,s"addHushRequested failure: $str"); false
  }
  def removeHushRequested(name: String) = await[Answer](ref, RemoveHushRequestedTrigger(name)) match {
    case Success(id) => true
    case Failure(str) => error(this,s"removeHushRequested failure: $str"); false    
  }
  
  def +=(signal: Double) = ref ! Signal(signal) 
  
  def !(any: Any) = {
    debug(this,s"$id, received: ${any.toString}")
    ref ! any
  }

  def ?(any: Any) = ref ? any
} 

object NeuronRef {
  def apply(id: String, treshold: Double, slope: Double, hushValue: HushValue, forgetting: ForgetTrait):NeuronRef = {
    debug(this,s"new neuronref $id with treshold $treshold and slope $slope")
    val ref = system.actorOf(Props(new Neuron(id, treshold, slope, hushValue, forgetting)), name=id)
    new NeuronRef(id, ref)
  }
}