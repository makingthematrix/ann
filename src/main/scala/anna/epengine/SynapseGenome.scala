package anna.epengine

import anna.Context
import anna.data.{Hush, SynapseData, SynapseTrait, SynapseWeight}
import anna.utils.RandomNumber
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

/**
 * Created by gorywoda on 27.12.14.
 */
class SynapseGenome(var neuronId: String, var weight: SynapseTrait){
  def toJson = writePretty(this)
  def data = SynapseData(neuronId, weight)
  override def clone = new SynapseGenome(neuronId, weight)
}

object SynapseGenome {
  def apply(neuronId: String, weight: SynapseTrait):SynapseGenome = new SynapseGenome(neuronId, weight)
  def apply(gen: SynapseGenome):SynapseGenome = new SynapseGenome(gen.neuronId, gen.weight)
  def apply(data: SynapseData):SynapseGenome = new SynapseGenome(data.neuronId, data.weight)
  def apply(neuronId: String, weight: Double):SynapseGenome = apply(neuronId, SynapseWeight(weight))

  def build(neuronId: String) = {
    val newSynapse = apply(neuronId, Hush())
    Probability.performRandom(
      (Context().hushProbability, () => {}),
      (Context().fullWeightProbability, () => { newSynapse.weight = SynapseWeight(1.0)}),
      (1.0 - Context().fullWeightProbability - Context().hushProbability - Context().invertSynapseProbability,
        () => { newSynapse.weight = SynapseWeight(RandomNumber(Context().weightRange))})
    )
    newSynapse
  }

  def fromJson(jsonStr: String) = read[SynapseGenome](jsonStr)
}