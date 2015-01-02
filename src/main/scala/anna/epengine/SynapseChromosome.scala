package anna.epengine

import anna.data.{SynapseData, SynapseTrait, SynapseWeight}

/**
 * Created by gorywoda on 27.12.14.
 */
case class SynapseChromosome(data: SynapseData){
  lazy val neuronId = data.neuronId
  lazy val weight = data.weight
}

object SynapseChromosome {
  def apply(neuronId: String, weight: SynapseTrait):SynapseChromosome = SynapseChromosome(SynapseData(neuronId, weight))
  def apply(neuronId: String, weight: Double):SynapseChromosome = apply(neuronId, SynapseWeight(weight))
}