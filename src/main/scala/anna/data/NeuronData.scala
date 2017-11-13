package anna.data

import anna.Context
import anna.async.Neuron.{InitialState, StartActive}
import anna.async._
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

case class NeuronData(id: String,
                      threshold: Double,
                      silenceIterations: Int,
                      initialState: InitialState,
                      synapses: List[SynapseData],
                      neuronType: NeuronType){
  def withId(id: String) = copy(id = id)
  def withThreshold(threshold: Double) = copy(threshold = threshold)
  def withSilenceIterations(silenceIterations: Int) = copy(silenceIterations = silenceIterations)
  def withInitialState(initialState: InitialState) = copy(initialState = initialState)
  def withSynapses(synapses: List[SynapseData]) = copy(synapses = synapses)
  def withoutSynapses = withSynapses(Nil)
  def withNeuronType(neuronType: NeuronType) = copy(neuronType = neuronType)

  def toJson = writePretty(this)

  def isConnectedTo(id: String) = synapses.find(_.neuronId == id) != None
}

object NeuronData {
  def apply(id: String, threshold: Double, silenceIterations: Int, initialState: InitialState, synapses: List[SynapseData]):NeuronData
  = apply(id, threshold, silenceIterations, initialState, synapses, NeuronTypeStandard())

  def apply(id: String, silenceIterations: Int):NeuronData
    = apply(id, 0.0, silenceIterations, StartActive, Nil, NeuronTypeDummy())
  
  def apply(id: String):NeuronData = apply(id, 0.0, Context().silenceIterations, StartActive, Nil, NeuronTypeSilencing())

  def fromJson(jsonStr: String) = read[NeuronData](jsonStr)
}