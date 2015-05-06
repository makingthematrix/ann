package anna.epengine

import anna.Context
import anna.data.{Hush, SynapseData, SynapseTrait, SynapseWeight}
import anna.utils.RandomNumber
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

/**
 * Created by gorywoda on 27.12.14.
 */
class SynapseGenome(private var _data: SynapseData){
  def neuronId = _data.neuronId
  def weight = _data.weight
  def data = _data

  def mutate() = Probability.performRandom(
    (Context().hushProbability, setWeightToHush _),
    (1.0 - Context().fullWeightProbability - Context().hushProbability, mutateWeight _),
    (Context().fullWeightProbability, setWeightToFull _)
  )

  private def setWeightToHush():Unit = {
    _data = _data.withWeight(Hush())
  }

  private def setWeightToFull():Unit = {
    _data = _data.withWeight(SynapseWeight(1.0))
  }

  private def mutateWeight():Unit = {
    _data = _data.weight match {
      case SynapseWeight(avoidWeight) => _data.withWeight(SynapseWeight(Context().weightRange.choose(RandomNumber(), avoidWeight)))
      case _ => _data.withWeight(SynapseWeight(RandomNumber(Context().weightRange)))
    }
  }

  def toJson = writePretty(this)
}

object SynapseGenome {
  def apply(data: SynapseData):SynapseGenome = new SynapseGenome(data)
  def apply(neuronId: String, weight: SynapseTrait):SynapseGenome = SynapseGenome(SynapseData(neuronId, weight))
  def apply(neuronId: String, weight: Double):SynapseGenome = apply(neuronId, SynapseWeight(weight))

  def build(neuronId: String) = {
    val nch = apply(neuronId, Hush())
    nch.mutate()
    nch
  }

  def fromJson(jsonStr: String) = read[SynapseGenome](jsonStr)
}