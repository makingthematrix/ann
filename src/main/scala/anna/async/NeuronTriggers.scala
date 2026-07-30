package anna.async

import anna.async.NeuronTriggers.Trigger
import scala.collection.mutable

trait NeuronTriggers {
  private val afterFire = mutable.ListBuffer[(String, Trigger)]()
  private val signalIgnored = mutable.ListBuffer[(String, Trigger)]()
  private val silenceRequested = mutable.ListBuffer[(String, Trigger)]()
  
  private def add(id: String, f: Trigger, triggers: mutable.ListBuffer[(String, Trigger)]): Option[Trigger] =
    if(triggers.exists(_._1 == id)) throw new IllegalArgumentException(s"There was already registered a trigger $id")
    else { triggers.addOne((id, f)); Some(f) }

  private def is(id: String, triggers: mutable.ListBuffer[(String, Trigger)]): Boolean = triggers.exists(_._1 == id)
  private def remove(id: String, triggers: mutable.ListBuffer[(String, Trigger)]): Option[Trigger] = {
    val t = triggers.find(_._1 == id).map(_._2)
    triggers.filterInPlace(_._1 != id)
    t
  }
  private def clear(triggers: mutable.ListBuffer[(String, Trigger)]): Unit = triggers.clear()
  private def trigger(triggers: mutable.ListBuffer[(String, Trigger)]): Unit = {
    triggers.foreach( _._2() )
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
