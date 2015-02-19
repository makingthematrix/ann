package anna.epengine

import anna.utils.DoubleRange._
import anna.data.{Hush, SynapseData, SynapseTrait, SynapseWeight}
import anna.utils.RandomNumber

/**
 * Created by gorywoda on 27.12.14.
 */
class SynapseGenome(private var _data: SynapseData){
  import SynapseGenome._

  def neuronId = _data.neuronId
  def weight = _data.weight
  def data = _data

  def mutate() = Probability.performRandom(
    (hushProbability, setWeightToHush _),
    (1.0 - fullWeightProbability - hushProbability, mutateWeight _),
    (fullWeightProbability, setWeightToFull _)
  )

  private def setWeightToHush():Unit = {
    _data = _data.withWeight(Hush)
  }

  private def setWeightToFull():Unit = {
    _data = _data.withWeight(SynapseWeight(1.0))
  }

  private def mutateWeight():Unit = {
    _data = _data.weight match {
      case SynapseWeight(avoidWeight) => _data.withWeight(SynapseWeight(weightRange.choose(RandomNumber(), avoidWeight)))
      case _ => _data.withWeight(SynapseWeight(RandomNumber(weightRange)))
    }
  }
}

object SynapseGenome {
  var weightRange = 0.01<=>1.0
  var hushProbability = Probability(0.05)
  var fullWeightProbability = Probability(0.2)

  def apply(data: SynapseData):SynapseGenome = new SynapseGenome(data)
  def apply(neuronId: String, weight: SynapseTrait):SynapseGenome = SynapseGenome(SynapseData(neuronId, weight))
  def apply(neuronId: String, weight: Double):SynapseGenome = apply(neuronId, SynapseWeight(weight))

  def toss(neuronId: String) = {
    val nch = apply(neuronId, Hush)
    nch.mutate()
    nch
  }
}