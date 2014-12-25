package anna.data

sealed trait SynapseTrait extends Any
case class SynapseWeight(weight: Double) extends AnyVal with SynapseTrait

case object Hush extends SynapseTrait

class SynapseData(val neuronId: String, val weight: SynapseTrait)

object SynapseData {
  def apply(neuronId: String, weight: SynapseTrait) = new SynapseData(neuronId, weight)
}