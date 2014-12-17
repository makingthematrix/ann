package main.data

case class HushValue(iterations: Int = 1) extends AnyVal
  
sealed trait ForgetTrait extends Any
case class ForgetValue(value: Double) extends AnyVal with ForgetTrait 
case object ForgetAll extends ForgetTrait
case object DontForget extends ForgetTrait

class NeuronData(
    val id: String, 
    val threshold: Double, 
    val slope: Double, 
    val hushValue: HushValue, 
    val forgetting: ForgetTrait,
    val synapses: List[SynapseData]
) 

object NeuronData {
  def apply(id: String, threshold: Double, slope: Double, hushValue: HushValue, forgetting: ForgetTrait, synapses: List[SynapseData])
    = new NeuronData(id, threshold, slope, hushValue, forgetting, synapses)
}