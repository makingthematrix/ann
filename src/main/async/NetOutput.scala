package main.async

import NeuronTriggers.Trigger
import main.async.logger.LOG.debug

class NetOutput(val name: String, val net: NetRef) {
  def getId(index: Int):String = ids(index)
  lazy val ids = net.outputIds
  lazy val size = net.outputSize
  
  def find(id: String) = net.outputIds.contains(id) match {
    case true => net.find(id).neuronOpt.get
    case false => throw new IllegalArgumentException(s"There is no output neuron with id $id")
  }
 
  def addAfterFireTrigger(id: String)(f: => Any) = find(id).addAfterFireTrigger(name+"_output_"+id)(f)
  def addAfterFireTrigger(neuron: Neuron)(f: => Any) = neuron.addAfterFireTrigger(name+"_output_"+neuron.id, () => f)

  def addDebug(id: String, info: String) = addAfterFireTrigger(id){ debug(this, info ) }
}

object NetOutput {
  def apply(name: String, net: NetRef) = new NetOutput(name, net)
}