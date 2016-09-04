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
    neuronType: NeuronType,
    friends: Set[String]
){
  def withId(id: String) = copy(id = id)
  def withThreshold(threshold: Double) = copy(threshold = threshold)
  def withHushValue(hushValue: HushValue) = copy(hushValue = hushValue)
  def withSynapses(synapses: List[SynapseData]) = copy(synapses = synapses)
  def withoutSynapses = withSynapses(Nil)
  def withNeuronType(neuronType: NeuronType) = copy(neuronType = neuronType)
  def withFriends(friends: Set[String]) = copy(friends = friends)
  def withoutFriends = withFriends(Set.empty[String])

  def toJson = writePretty(this)

  def isConnectedTo(id: String) = synapses.find(_.neuronId == id) != None
}

object NeuronData {
  def apply(id: String,
            threshold: Double,
            hushValue: HushValue,
            synapses: List[SynapseData],
            friends: Set[String]
           ):NeuronData
  = apply(id, threshold, hushValue, synapses, NeuronTypeStandard(), friends)

  def apply(id: String, 
            threshold: Double,
            hushValue: HushValue,
            synapses: List[SynapseData]):NeuronData
    = apply(id, threshold, hushValue, synapses, NeuronTypeStandard(), Set.empty[String])

  def apply(id: String,  
            hushValue: HushValue):NeuronData
    = apply(id, 0.0, hushValue, Nil, NeuronTypeDummy(), Set.empty[String])
  
  def apply(id: String):NeuronData
    = apply(id, 0.0, Context().hushValue, Nil, NeuronTypeHush(), Set.empty[String])

  def fromJson(jsonStr: String) = read[NeuronData](jsonStr)
}