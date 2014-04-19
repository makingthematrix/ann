package main

import scala.collection.mutable

class NetOutput(val name: String, val net: Net) {
  def ids = net.outputIds
  def size = net.outputSize
  
  private def find(id: Long) = net.outputIds.contains(id) match {
    case true => net.find(id).get
    case false => throw new IllegalArgumentException(s"There is no output neuron with id $id")
  }
  
  def addTrigger(id: Long, f:(Neuron) => Unit) = find(id).addAfterTickTrigger(name+"_output_"+id, f)
  def addTrigger(alias: String, f:(Neuron) => Unit) = aliases.get(alias) match {
    case Some(n) => n.addAfterTickTrigger(name+"_output_"+alias, f)
    case None => throw new IllegalArgumentException(s"There is no output neuron with alias $alias")
  }
  
  private val aliases = mutable.Map[String,Neuron]()
  def regAlias(alias: String, id: Long) = aliases.put(alias, find(id))
  
  def addInfo(id: Long, info: String) = addTrigger(id, (_: Neuron) => println( info ) )
  def addInfo(alias: String, info: String) = addTrigger(alias, (_: Neuron) => println( info ) )
}

object NetOutput {
  def apply(name: String, net: Net) = new NetOutput(name, net)
}