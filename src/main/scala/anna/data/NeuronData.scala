package anna.data

import anna.Context
import anna.async._
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

case class NeuronData(
    id: String,
    threshold: Double,
    slope: Double,
    hushValue: HushValue,
    forgetting: ForgetTrait,
    synapses: List[SynapseData],
    tickTimeMultiplier: Double,
    neuronType: NeuronType,
    activationFunctionName: String
){
  def withId(id: String) = copy(id = id)
  def withThreshold(threshold: Double) = copy(threshold = threshold)
  def withSlope(slope: Double) = copy(slope = slope)
  def withHushValue(hushValue: HushValue) = copy(hushValue = hushValue)
  def withForgetting(forgetting: ForgetTrait) = copy(forgetting = forgetting)
  def withSynapses(synapses: List[SynapseData]) = copy(synapses = synapses)
  def withoutSynapses = withSynapses(Nil)
  def withTickTimeMultiplier(tickTimeMultiplier: Double) = copy(tickTimeMultiplier = tickTimeMultiplier)
  def withNeuronType(neuronType: NeuronType) = copy(neuronType = neuronType)
  def withActivationFunctionName(activationFunctionName: String) = copy(activationFunctionName = activationFunctionName)

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
            tickTimeMultiplier: Double,
            activationFunctionName: String):NeuronData
    = apply(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, NeuronTypeStandard(), activationFunctionName)

  def apply(id: String, 
            threshold: Double, 
            slope: Double, 
            hushValue: HushValue,
            forgetting: ForgetTrait,
            tickTimeMultiplier: Double,
            activationFunctionName: String):NeuronData
    = apply(id, threshold, slope, hushValue, forgetting, Nil, tickTimeMultiplier, NeuronTypeStandard(), activationFunctionName)

  def apply(id: String,  
            hushValue: HushValue,
            tickTimeMultiplier: Double):NeuronData
    = apply(id, 0.0, Context().slope, hushValue, ForgetAll(), Nil, tickTimeMultiplier, NeuronTypeDummy(), ActivationFunction.UNUSED)
  
  def apply(id: String):NeuronData
    = apply(id, 0.0, Context().slope, Context().hushValue, ForgetAll(), Nil, 1.0, NeuronTypeHush(), ActivationFunction.UNUSED)

  def fromJson(jsonStr: String) = read[NeuronData](jsonStr)
}