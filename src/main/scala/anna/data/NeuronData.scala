package anna.data

import anna.async.NeuronType

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
    val synapses: List[SynapseData],
    val neuronType: NeuronType.Value
){
  def withSynapses(synapses: List[SynapseData]) = NeuronData(id, threshold, slope, hushValue, forgetting, synapses, neuronType)
} 

object NeuronData {
  def apply(id: String, 
            threshold: Double, 
            slope: Double, 
            hushValue: HushValue, 
            forgetting: ForgetTrait, 
            synapses: List[SynapseData],
            neuronType: NeuronType.Value):NeuronData
    = new NeuronData(id, threshold, slope, hushValue, forgetting, synapses, neuronType)
  
  def apply(id: String, 
            threshold: Double, 
            slope: Double, 
            hushValue: HushValue, 
            forgetting: ForgetTrait, 
            synapses: List[SynapseData]):NeuronData
    = apply(id, threshold, slope, hushValue, forgetting, synapses, NeuronType.STANDARD)

  def apply(id: String, 
            threshold: Double, 
            slope: Double, 
            hushValue: HushValue, 
            forgetting: ForgetTrait):NeuronData
    = apply(id, threshold, slope, hushValue, forgetting, Nil, NeuronType.STANDARD)

  def apply(id: String,  
            hushValue: HushValue):NeuronData
    = apply(id, 0.0, 0.0, hushValue, ForgetAll, Nil, NeuronType.DUMMY)
  
  def apply(id: String):NeuronData
    = apply(id, 0.0, 0.0, HushValue(), ForgetAll, Nil, NeuronType.HUSH)

}