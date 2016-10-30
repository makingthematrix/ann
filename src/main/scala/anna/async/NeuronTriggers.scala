package anna.async

import anna.async.NeuronTriggers.Trigger
import scala.collection.mutable

import anna.logger.LOG

trait NeuronTriggers {
  private val afterFire = mutable.Map[String, Trigger]()
  private val signalIgnored = mutable.Map[String, Trigger]()
  private val silenceRequested = mutable.Map[String, Trigger]()
  
  private def add(id: String, f: Trigger, triggers: mutable.Map[String, Trigger]) =
    if(triggers.contains(id)) throw new IllegalArgumentException(s"There was already registered a trigger $id")
    else {
      LOG.debug(s"registering trigger $id")
      triggers.put(id, f)
    }

  private def is(id: String, triggers: mutable.Map[String, Trigger]) = triggers.contains(id) 
  private def remove(id: String, triggers: mutable.Map[String, Trigger]) = triggers.remove(id)
  private def clear(triggers: mutable.Map[String, Trigger]) = triggers.clear()
  private def trigger(triggers: mutable.Map[String, Trigger]) = {
    LOG.debug("triggering: " + triggers.keys.toString())
    triggers.values.foreach( _() )
  }
  
  def addAfterFire(id: String, f: Trigger) = add(id, f, afterFire)
  def isAfterFire(id: String) = is(id, afterFire)
  def removeAfterFire(id: String) = remove(id, afterFire)
  def clearAfterFire() = clear(afterFire)
  def triggerAfterFire() = trigger(afterFire)

  def addSignalIgnored(id: String, f: Trigger) = add(id, f, signalIgnored)
  def isSignalIgnored(id: String) = is(id, signalIgnored)
  def removeSignalIgnored(id: String) = remove(id, signalIgnored)
  def clearSignalIgnored() = clear(signalIgnored) 
  def triggerSignalIgnored() = trigger(signalIgnored)
  
  def addSilenceRequested(id: String, f: Trigger) = add(id, f, silenceRequested)
  def isSilenceRequested(id: String) = is(id, silenceRequested)
  def removeSilenceRequested(id: String) = remove(id, silenceRequested)
  def clearSilenceRequested() = clear(silenceRequested)
  def triggerSilenceRequested() = trigger(silenceRequested)

  def removeAllTriggers() = {
    afterFire.clear()
    signalIgnored.clear()
    silenceRequested.clear()
  }
}

object NeuronTriggers {
  type Trigger = () => Any
}
