package main.async

class AkkaNetOutput(val name: String, val net: NetRef) {
  def getId(index: Int):String = ids(index)
  lazy val ids = net.outputIds
  lazy val size = net.outputSize
  
  def find(id: String) = net.outputIds.contains(id) match {
    case true => net.find(id).neuronOpt.get
    case false => throw new IllegalArgumentException(s"There is no output neuron with id $id")
  }
 
  def addAfterFireTrigger(id: String, f:(AkkaNeuron) => Any):Unit = find(id).addAfterFireTrigger(name+"_output_"+id, f)
  def addAfterFireTrigger(neuron: AkkaNeuron, f:(AkkaNeuron) => Any):Unit = neuron.addAfterFireTrigger(name+"_output_"+neuron.id, f)

  def addInfo(id: String, info: String) = addAfterFireTrigger(id, (_: AkkaNeuron) => println( info ) )
}

object AkkaNetOutput {
  def apply(name: String, net: NetRef) = new AkkaNetOutput(name, net)
}