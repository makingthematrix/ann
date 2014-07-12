package main

import scala.collection.mutable

class NetOutput(val name: String, val net: Net) {
  def ids = net.outputIds
  def size = net.outputSize
  
  def find(id: String) = net.outputIds.contains(id) match {
    case true => net.find(id).get
    case false => throw new IllegalArgumentException(s"There is no output neuron with id $id")
  }
 
  def addAfterFireTrigger(id: String, f:(NeuronLike) => Any):Unit = find(id).addAfterFireTrigger(name+"_output_"+id, f)
  def addAfterFireTrigger(neuron: Neuron, f:(NeuronLike) => Any):Unit = neuron.addAfterFireTrigger(name+"_output_"+neuron.id, f)
  
  def addAfterTickTrigger(id: String, f:(NeuronLike) => Any):Unit = find(id).addAfterTickTrigger(name+"_output_"+id, f)
  def addAfterTickTrigger(neuron: Neuron, f:(NeuronLike) => Any):Unit = neuron.addAfterTickTrigger(name+"_output_"+neuron.id, f)
  
  def addInfo(id: String, info: String) = addAfterFireTrigger(id, (_: NeuronLike) => println( info ) )
}

object NetOutput {
  def apply(name: String, net: Net) = new NetOutput(name, net)
}