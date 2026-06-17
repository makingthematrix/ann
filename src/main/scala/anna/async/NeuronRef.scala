package anna.async

import akka.actor.{ActorRef, actorRef2Scala}
import anna.async.Messages._
import anna.logger.LOG._
import anna.utils.Utils.await
import anna.async.NeuronTriggers.Trigger

class NeuronRef(val id: String, val ref: ActorRef) {
  def info: NeuronInfo = await[NeuronInfo](ref, GetData)
  def getSynapses: Seq[Synapse] = await[MsgSynapses](ref, GetSynapses).synapses
  def setSynapses(synapses: Seq[Synapse]): Unit = if(synapses.nonEmpty) ref ! SetSynapses(synapses)

  def requestSilence(): Unit = {
    debug(this, s"request silence for $id")
    ref ! SilenceRequest
  }

  def requestWakeUp(): Unit = {
    debug(this, s"request wake up for $id")
    ref ! WakeRequest
  }
  
  protected def calculateOutput: Double = Double.NaN // we don't do that here
  
  def addAfterFire(triggerId: String)(f: => Any): Boolean = await[Answer](ref, AddAfterFireTrigger(triggerId, () => f)) match {
    case Success(_) => true
    case Failure(str) => error(this,s"addAfterFire failure: $str"); false
    case other: Answer => error(this, s"Wrong answer: $other"); false
  }
  def removeAfterFire(name: String): Boolean = await[Answer](ref, RemoveAfterFireTrigger(name)) match {
    case Success(_) => true
    case Failure(str) => error(this,s"removeAfterFire failure: $str"); false
    case other: Answer => error(this, s"Wrong answer: $other"); false
  }
  def addSilenceRequested(triggerId: String)(f: => Any): Boolean = await[Answer](ref, AddSilenceRequestedTrigger(triggerId, () => f)) match {
    case Success(_) => true
    case Failure(str) => error(this,s"addSilenceRequested failure: $str"); false
    case other: Answer => error(this, s"Wrong answer: $other"); false
  }
  def removeSilenceRequested(name: String): Boolean = await[Answer](ref, RemoveSilenceRequestedTrigger(name)) match {
    case Success(_) => true
    case Failure(str) => error(this,s"removeSilenceRequested failure: $str"); false
    case other: Answer => error(this, s"Wrong answer: $other"); false
  }
  def addSignalIgnored(triggerId: String)(f: => Any): Boolean = await[Answer](ref, AddSignalIgnoredTrigger(triggerId, () => f)) match {
    case Success(_) => true
    case Failure(str) => error(this,s"addSignalIgnored failure: $str"); false
    case other: Answer => error(this, s"Wrong answer: $other"); false
  }
  def removeSignalIgnored(name: String): Boolean = await[Answer](ref, RemoveSignalIgnoredTrigger(name)) match {
    case Success(_) => true
    case Failure(str) => error(this,s"removeSignalIgnored failure: $str"); false
    case other: Answer => error(this, s"Wrong answer: $other"); false
  }

  def removeAllTriggers(): Boolean = await[Answer](ref, RemoveAllTriggers) match {
    case Success(_) => true
    case Failure(str) => error(this,s"removeAllTriggers failure: $str"); false
    case other: Answer => error(this, s"Wrong answer: $other"); false
  }

  def reset(): Boolean = await[Answer](ref, Reset) match {
    case Success(_) => true
    case Failure(str) => error(this,s"reset failure: $str"); false
    case other: Answer => error(this, s"Wrong answer: $other"); false
  }
  
  def +=(signal: Double): Unit = ref ! Signal(signal, id)
  def !(any: Any): Unit = ref ! any
}
