package anna.async

import akka.actor.{ActorRef, actorRef2Scala}
import anna.async.Messages._
import anna.logger.LOG._
import anna.utils.Utils.await
import anna.async.NeuronTriggers.Trigger

class NeuronRef(val id: String, val ref: ActorRef) {
  def info = await[NeuronInfo](ref, GetData)
  def getSynapses = await[MsgSynapses](ref, GetSynapses).synapses
  def setSynapses(synapses: Seq[Synapse]) = if(synapses.nonEmpty) ref ! SetSynapses(synapses)

  def requestSilence() = {
    debug(this, s"request silence for $id")
    ref ! SilenceRequest
  }

  def requestWakeUp() = {
    debug(this, s"request wake up for $id")
    ref ! WakeRequest
  }
  
  protected def calculateOutput = Double.NaN // we don't do that here 
  
  def addAfterFire(triggerId: String)(f: => Any) = await[Answer](ref, AddAfterFireTrigger(triggerId, () => f)) match {
    case Success(id) => true
    case Failure(str) => error(this,s"addAfterFire failure: $str"); false
  }
  def removeAfterFire(name: String) = await[Answer](ref, RemoveAfterFireTrigger(name)) match {
    case Success(id) => true
    case Failure(str) => error(this,s"removeAfterFire failure: $str"); false    
  }
  def addSilenceRequested(triggerId: String)(f: => Any) = await[Answer](ref, AddSilenceRequestedTrigger(triggerId, () => f)) match {
    case Success(id) => true
    case Failure(str) => error(this,s"addSilenceRequested failure: $str"); false
  }
  def removeSilenceRequested(name: String) = await[Answer](ref, RemoveSilenceRequestedTrigger(name)) match {
    case Success(id) => true
    case Failure(str) => error(this,s"removeSilenceRequested failure: $str"); false
  }
  def addSignalIgnored(triggerId: String)(f: => Any) = await[Answer](ref, AddSignalIgnoredTrigger(triggerId, () => f)) match {
    case Success(id) => true
    case Failure(str) => error(this,s"addSignalIgnored failure: $str"); false
  }
  def removeSignalIgnored(name: String) = await[Answer](ref, RemoveSignalIgnoredTrigger(name)) match {
    case Success(id) => true
    case Failure(str) => error(this,s"removeSignalIgnored failure: $str"); false
  }

  def removeAllTriggers() = await[Answer](ref, RemoveAllTriggers) match {
    case Success(id) => true
    case Failure(str) => error(this,s"removeAllTriggers failure: $str"); false
  }

  def reset() = await[Answer](ref, Reset) match {
    case Success(id) => true
    case Failure(str) => error(this,s"reset failure: $str"); false
  }
  
  def +=(signal: Double) = ref ! Signal(signal, id)
  def !(any: Any) = ref ! any
}
