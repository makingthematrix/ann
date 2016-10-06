package anna.data

import anna.Context
import anna.async._
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

case class NeuronData(
    id: String,
    threshold: Double,
    hushValue: HushValue,
    synapses: List[SynapseData],
    neuronType: NeuronType
){
  def withId(id: String) = copy(id = id)
  def withThreshold(threshold: Double) = copy(threshold = threshold)
  def withHushValue(hushValue: HushValue) = copy(hushValue = hushValue)
  def withSynapses(synapses: List[SynapseData]) = copy(synapses = synapses)
  def withoutSynapses = withSynapses(Nil)
  def withNeuronType(neuronType: NeuronType) = copy(neuronType = neuronType)

  def toJson = writePretty(this)

  def isConnectedTo(id: String) = synapses.find(_.neuronId == id) != None
}

object NeuronData {
  def apply(id: String,
            threshold: Double,
            hushValue: HushValue,
            synapses: List[SynapseData]
           ):NeuronData
  = apply(id, threshold, hushValue, synapses, NeuronTypeStandard())

  def apply(id: String,  
            hushValue: HushValue):NeuronData
    = apply(id, 0.0, hushValue, Nil, NeuronTypeDummy())
  
  def apply(id: String):NeuronData
    = apply(id, 0.0, Context().hushValue, Nil, NeuronTypeHush())

  def fromJson(jsonStr: String) = read[NeuronData](jsonStr)
}