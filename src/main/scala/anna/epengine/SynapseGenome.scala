package anna.epengine

import anna.Context
import anna.data.{Hush, SynapseData, SynapseTrait, SynapseWeight}
import anna.utils.RandomNumber
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}
import anna.logger.LOG._

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
    debug(s"MUTATION: ... setWeightToHush in a synapse connecting to $neuronId from ${_data.weight}")
    _data = _data.withWeight(Hush())
  }

  private def setWeightToFull():Unit = {
    debug(s"MUTATION: ... setWeightToFull in a synapse connecting to $neuronId from ${_data.weight}")
    _data = _data.withWeight(SynapseWeight(1.0))
  }

  private def mutateWeight():Unit = {
    val newWeight = _data.weight match {
      case SynapseWeight(avoidWeight) =>
        SynapseWeight(Context().weightRange.choose(RandomNumber(), avoidWeight))
      case _ =>
        SynapseWeight(RandomNumber(Context().weightRange))
    }
    debug(s"MUTATION: ... mutateWeight in a synapse connecting to $neuronId -> from ${_data.weight} to $newWeight")
    _data = _data.withWeight(newWeight)
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