package main

import scala.collection.mutable

class NetOutput(val name: String, val net: Net) {
  def ids = net.outputIds
  def size = net.outputSize
  
  private val aliases = mutable.Map[String,Neuron]()
  def regAlias(alias: String, id: Long) = aliases.put(alias, find(id))
    
  def find(id: Long) = net.outputIds.contains(id) match {
    case true => net.find(id).get
    case false => throw new IllegalArgumentException(s"There is no output neuron with id $id")
  }
  def find(alias: String) = aliases.contains(alias) match {
    case true => aliases(alias)
    case false => throw new IllegalArgumentException(s"There is no output neuron with alias $alias")
  }
 
  def addAfterFireTrigger(id: Long, f:(Neuron) => Any):Unit = find(id).addAfterFireTrigger(name+"_output_"+id, f)
  def addAfterFireTrigger(alias: String, f:(Neuron) => Any):Unit = aliases.get(alias) match {
    case Some(n) => n.addAfterFireTrigger(name+"_output_"+alias, f)
    case None => throw new IllegalArgumentException(s"There is no output neuron with alias $alias")
  }
  def addAfterFireTrigger(neuron: Neuron, f:(Neuron) => Any):Unit = neuron.addAfterFireTrigger(name+"_output_"+neuron.id, f)
  
  def addAfterTickTrigger(id: Long, f:(Neuron) => Any):Unit = find(id).addAfterTickTrigger(name+"_output_"+id, f)
  def addAfterTickTrigger(alias: String, f:(Neuron) => Any):Unit = aliases.get(alias) match {
    case Some(n) => n.addAfterTickTrigger(name+"_output_"+alias, f)
    case None => throw new IllegalArgumentException(s"There is no output neuron with alias $alias")
  }
  def addAfterTickTrigger(neuron: Neuron, f:(Neuron) => Any):Unit = neuron.addAfterTickTrigger(name+"_output_"+neuron.id, f)
  
  def addInfo(id: Long, info: String) = addAfterFireTrigger(id, (_: Neuron) => println( info ) )
  def addInfo(alias: String, info: String) = addAfterFireTrigger(alias, (_: Neuron) => println( info ) )
}

object NetOutput {
  def apply(name: String, net: Net) = new NetOutput(name, net)
}