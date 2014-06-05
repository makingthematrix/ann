package main

import scala.collection.mutable

object LOG {
  private val _allowedIds = mutable.Set[String]()
  
  private def log(str: String):Unit = println(str)
  
  def allow(id: String):Unit = _allowedIds += id
  def allow(implicit n: Neuron):Unit = allow(n.id)
  
  def allowedIds = _allowedIds.toSet
  def clearAllowedIds() = _allowedIds.clear()
  def removeAllowedId(id: String) = _allowedIds -= id
  
  def +=(str: String)(implicit n: Neuron) = log(str, n)
  def log(str: String, n: Neuron):Unit = if(_allowedIds.contains(n.id)) log(str)
}