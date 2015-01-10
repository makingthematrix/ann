package anna.epengine

import anna.async.NeuronType
import anna.data.{ForgetTrait, HushValue, NeuronData, SynapseData}

/**
 * Created by gorywoda on 28.12.14.
 */
class NeuronChromosome(private var data: NeuronData) {
  def id = data.id
  def threshold = data.threshold
  def slope = data.slope
  def hushValue = data.hushValue
  def forgetting = data.forgetting
  def synapses = data.synapses
  def tickTimeMultiplier = data.tickTimeMultiplier
  def neuronType = data.neuronType
  def neuron = data

  def addSynapse(synapseChromosome: SynapseChromosome): Unit ={
    data = data.withSynapses(synapseChromosome.synapse :: data.synapses)
  }

  def isConnectedTo(id: String) = synapses.find(_.neuronId == id) != None
}

object NeuronChromosome {
  def apply(data: NeuronData):NeuronChromosome = new NeuronChromosome(data)
  def apply(id: String,
            threshold: Double,
            slope: Double,
            hushValue: HushValue,
            forgetting: ForgetTrait,
            synapses: List[SynapseData],
            tickTimeMultiplier: Double,
            neuronType: NeuronType.Value):NeuronChromosome =
    NeuronChromosome(NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, neuronType))
}
