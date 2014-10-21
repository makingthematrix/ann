package main.async

import Context.Trigger

class NetOutput(val name: String, val net: NetRef) {
  def getId(index: Int):String = ids(index)
  lazy val ids = net.outputIds
  lazy val size = net.outputSize
  
  def find(id: String) = net.outputIds.contains(id) match {
    case true => net.find(id).neuronOpt.get
    case false => throw new IllegalArgumentException(s"There is no output neuron with id $id")
  }
 
  def addAfterFireTrigger(id: String, f: Trigger) = find(id).addAfterFireTrigger(name+"_output_"+id, f)
  def addAfterFireTrigger(neuron: Neuron, f: Trigger) = neuron.addAfterFireTrigger(name+"_output_"+neuron.id, f)

  def addInfo(id: String, info: String) = addAfterFireTrigger(id, () => println( info ) )
}

object NetOutput {
  def apply(name: String, net: NetRef) = new NetOutput(name, net)
}