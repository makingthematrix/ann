package anna.async

import anna.async.NeuronTriggers.Trigger
import scala.collection.mutable

import anna.logger.LOG

trait NeuronTriggers {
  private val afterFire = mutable.ListMap[String, Trigger]()
  private val signalIgnored = mutable.ListMap[String, Trigger]()
  private val silenceRequested = mutable.ListMap[String, Trigger]()
  
  private def add(id: String, f: Trigger, triggers: mutable.ListMap[String, Trigger]) =
    if(triggers.contains(id)) throw new IllegalArgumentException(s"There was already registered a trigger $id")
    else { triggers.put(id, f) }

  private def is(id: String, triggers: mutable.ListMap[String, Trigger]) = triggers.contains(id) 
  private def remove(id: String, triggers: mutable.ListMap[String, Trigger]) = triggers.remove(id)
  private def clear(triggers: mutable.ListMap[String, Trigger]): Unit = triggers.clear()
  private def trigger(triggers: mutable.ListMap[String, Trigger]): Unit = {
    triggers.values.foreach( f => f() )
  }
  
  def addAfterFire(id: String, f: Trigger): Option[Trigger] = add(id, f, afterFire)
  def isAfterFire(id: String): Boolean = is(id, afterFire)
  def removeAfterFire(id: String): Option[Trigger] = remove(id, afterFire)
  def clearAfterFire(): Unit = clear(afterFire)
  def triggerAfterFire(): Unit = trigger(afterFire)

  def addSignalIgnored(id: String, f: Trigger): Option[Trigger] = add(id, f, signalIgnored)
  def isSignalIgnored(id: String): Boolean = is(id, signalIgnored)
  def removeSignalIgnored(id: String): Option[Trigger] = remove(id, signalIgnored)
  def clearSignalIgnored(): Unit = clear(signalIgnored)
  def triggerSignalIgnored(): Unit = trigger(signalIgnored)
  
  def addSilenceRequested(id: String, f: Trigger): Option[Trigger] = add(id, f, silenceRequested)
  def isSilenceRequested(id: String): Boolean = is(id, silenceRequested)
  def removeSilenceRequested(id: String): Option[Trigger] = remove(id, silenceRequested)
  def clearSilenceRequested(): Unit = clear(silenceRequested)
  def triggerSilenceRequested(): Unit = trigger(silenceRequested)

  def removeAllTriggers(): Unit = {
    afterFire.clear()
    signalIgnored.clear()
    silenceRequested.clear()
  }
}

object NeuronTriggers {
  type Trigger = () => Any
}
