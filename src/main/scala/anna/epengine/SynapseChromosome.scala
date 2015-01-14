package anna.epengine

import anna.utils.DoubleRange._
import anna.data.{Hush, SynapseData, SynapseTrait, SynapseWeight}

/**
 * Created by gorywoda on 27.12.14.
 */
class SynapseChromosome(private var data: SynapseData){
  def neuronId = data.neuronId
  def weight = data.weight
  def synapse = data

  var weightRange = 0.01<=>1.0
  var hushProbability = Probability(0.05)
  var fullWeightProbability = Probability(0.2)

  def mutate(): Unit ={
    val variedWeightProbability = fullWeightProbability - hushProbability
    data = Probability.chooseOne(hushProbability, variedWeightProbability, fullWeightProbability) match {
      case 0 => data.withWeight(Hush)
      case 1 => data.withWeight(SynapseWeight(weightRange.choose(RandomNumber())))
      case 2 => data.withWeight(SynapseWeight(1.0))
    }
  }
}

object SynapseChromosome {
  def apply(data: SynapseData):SynapseChromosome = new SynapseChromosome(data)
  def apply(neuronId: String, weight: SynapseTrait):SynapseChromosome = SynapseChromosome(SynapseData(neuronId, weight))
  def apply(neuronId: String, weight: Double):SynapseChromosome = apply(neuronId, SynapseWeight(weight))
}