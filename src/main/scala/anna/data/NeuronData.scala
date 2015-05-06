package anna.data

import anna.Context
import anna.async.{NeuronTypeDummy, NeuronTypeHush, NeuronTypeStandard, NeuronType}
import org.json4s.native.Serialization.{ read, writePretty }
import anna.utils.Utils.formats

case class NeuronData(
    id: String,
    threshold: Double,
    slope: Double,
    hushValue: HushValue,
    forgetting: ForgetTrait,
    synapses: List[SynapseData],
    tickTimeMultiplier: Double,
    neuronType: NeuronType
){
  def withId(id: String) =
    NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, neuronType)
  def withThreshold(threshold: Double) =
    NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, neuronType)
  def withSlope(slope: Double) =
    NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, neuronType)
  def withHushValue(hushValue: HushValue) =
    NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, neuronType)
  def withForgetting(forgetting: ForgetTrait) =
    NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, neuronType)
  def withSynapses(synapses: List[SynapseData]) =
    NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, neuronType)
  def withoutSynapses = withSynapses(Nil)
  def withTickTimeMultiplier(tickTimeMultiplier: Double) =
    NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, neuronType)
  def withNeuronType(neuronType: NeuronType) =
    NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, neuronType)

  def toJson = writePretty(this)

  def isConnectedTo(id: String) = synapses.find(_.neuronId == id) != None
}

object NeuronData {
  def apply(id: String, 
            threshold: Double, 
            slope: Double, 
            hushValue: HushValue, 
            forgetting: ForgetTrait,
            synapses: List[SynapseData],
            tickTimeMultiplier: Double):NeuronData
    = apply(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, NeuronTypeStandard())

  def apply(id: String, 
            threshold: Double, 
            slope: Double, 
            hushValue: HushValue,
            forgetting: ForgetTrait,
            tickTimeMultiplier: Double):NeuronData
    = apply(id, threshold, slope, hushValue, forgetting, Nil, tickTimeMultiplier, NeuronTypeStandard())

  def apply(id: String,  
            hushValue: HushValue,
            tickTimeMultiplier: Double):NeuronData
    = apply(id, 0.0, Context().slope, hushValue, ForgetAll(), Nil, tickTimeMultiplier, NeuronTypeDummy())
  
  def apply(id: String):NeuronData
    = apply(id, 0.0, Context().slope, Context().hushValue, ForgetAll(), Nil, 1.0, NeuronTypeHush())

  def fromJson(jsonStr: String) = read[NeuronData](jsonStr)
}