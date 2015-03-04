package anna.async

import akka.actor.{ActorRef, Props, actorRef2Scala}
import akka.pattern.ask
import anna.Context._
import anna.async.Messages._
import anna.data.{ForgetTrait, HushValue}
import anna.logger.LOG._
import anna.utils.Utils.await

class NeuronRef(val id: String, val ref: ActorRef) {
  def input = await[Msg](ref, GetInput).d
  def lastOutput = await[Msg](ref, GetLastOutput).d
  def getSynapses = await[MsgSynapses](ref, GetSynapses).synapses
  def setSynapses(synapses: Seq[Synapse]) = if(synapses.nonEmpty) ref ! SetSynapses(synapses)
  
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
  def !(any: Any) = ref ! any
  def ?(any: Any) = ref ? any
} 

object NeuronRef {
  def apply(id: String,
            threshold: Double,
            slope: Double,
            hushValue: HushValue,
            forgetting: ForgetTrait,
            tickTime: Long):NeuronRef = {
    debug(this,s"new neuronref $id with threshold $threshold and slope $slope")
    val ref = system.actorOf(Props(new Neuron(id, threshold, slope, hushValue, forgetting, tickTime)), name=id)
    new NeuronRef(id, ref)
  }
}