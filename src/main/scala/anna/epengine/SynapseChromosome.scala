package anna.epengine

import anna.data.{SynapseData, SynapseTrait, SynapseWeight}

/**
 * Created by gorywoda on 27.12.14.
 */
class SynapseChromosome(private var data: SynapseData){
  def neuronId = data.neuronId
  def weight = data.weight
  def synapse = data
}

object SynapseChromosome {
  def apply(data: SynapseData):SynapseChromosome = new SynapseChromosome(data)
  def apply(neuronId: String, weight: SynapseTrait):SynapseChromosome = SynapseChromosome(SynapseData(neuronId, weight))
  def apply(neuronId: String, weight: Double):SynapseChromosome = apply(neuronId, SynapseWeight(weight))
}