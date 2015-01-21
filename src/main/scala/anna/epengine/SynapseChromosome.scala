package anna.epengine

import anna.utils.DoubleRange._
import anna.data.{Hush, SynapseData, SynapseTrait, SynapseWeight}

/**
 * Created by gorywoda on 27.12.14.
 */
class SynapseChromosome(private var data: SynapseData){
  import SynapseChromosome._

  def neuronId = data.neuronId
  def weight = data.weight
  def synapse = data

  def mutate() = Probability.performRandom(
    (hushProbability, setWeightToHush _),
    (1.0 - fullWeightProbability - hushProbability, mutateWeight _),
    (fullWeightProbability, setWeightToFull _)
  )

  private def setWeightToHush():Unit = {
    data = data.withWeight(Hush)
  }

  private def setWeightToFull():Unit = {
    data = data.withWeight(SynapseWeight(1.0))
  }

  private def mutateWeight():Unit = {
    data = data.weight match {
      case SynapseWeight(avoidWeight) => data.withWeight(SynapseWeight(weightRange.choose(RandomNumber(), avoidWeight)))
      case _ => data.withWeight(SynapseWeight(weightRange.choose(RandomNumber())))
    }
  }
}

object SynapseChromosome {
  var weightRange = 0.01<=>1.0
  var hushProbability = Probability(0.05)
  var fullWeightProbability = Probability(0.2)

  def apply(data: SynapseData):SynapseChromosome = new SynapseChromosome(data)
  def apply(neuronId: String, weight: SynapseTrait):SynapseChromosome = SynapseChromosome(SynapseData(neuronId, weight))
  def apply(neuronId: String, weight: Double):SynapseChromosome = apply(neuronId, SynapseWeight(weight))
}