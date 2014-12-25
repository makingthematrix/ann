package anna.sync

import scala.collection.mutable

trait NeuronTriggers {
  type Trigger = () => Any
  protected val afterFireTriggers = mutable.Map[String, Trigger]()
  def addAfterFireTrigger(id: String, f: Trigger) = afterFireTriggers.contains(id) match {
    case false => afterFireTriggers.put(id, f)
    case true => throw new IllegalArgumentException(s"There was already registered an after fire trigger with id $id")
  } 
  def isAfterFireTrigger(id: String) = afterFireTriggers.contains(id)
  def removeAfterFireTrigger(id: String) = afterFireTriggers.remove(id)
  def clearAfterFireTriggers() = afterFireTriggers.clear

  protected val afterTickTriggers = mutable.Map[String, Trigger]()
  def addAfterTickTrigger(id: String, f: Trigger) = afterTickTriggers.contains(id) match {
    case false => afterTickTriggers.put(id, f)
    case true => throw new IllegalArgumentException(s"There was already registered a after tick trigger with id $id")
  } 
  def isTickFireTrigger(id: String) = afterTickTriggers.contains(id)
  def removeAfterTickTrigger(id: String) = afterTickTriggers.remove(id)
  def clearAfterTickTriggers() = afterTickTriggers.clear
  
  protected val tresholdPassedTriggers = mutable.Map[String, Trigger]()
  def addTresholdPassedTrigger(id: String, f: Trigger) = tresholdPassedTriggers.contains(id) match {
    case false => tresholdPassedTriggers.put(id, f)
    case true => throw new IllegalArgumentException("There was already registered a trigger with id " + id)
  } 
  def isTresholdPassedTrigger(id: String) = tresholdPassedTriggers.contains(id)
  def removeTresholdPassedTrigger(id: String) = tresholdPassedTriggers.remove(id)
  def clearTresholdPassedTriggers() = tresholdPassedTriggers.clear
  
  def clearAllTriggers() = {
    clearAfterFireTriggers()
    clearAfterTickTriggers()
    clearTresholdPassedTriggers()
  }
  
}