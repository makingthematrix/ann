package anna.data

import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

import scala.language.implicitConversions

final case class SynapseData(neuronId: String, weight: SynapseTrait){
  def withId(neuronId: String) = SynapseData(neuronId, weight)
  def withWeight(weight: SynapseTrait) = SynapseData(neuronId, weight)

  def toJson: String = writePretty(this)
}

object SynapseData {
  def apply(neuronId: String, weight: Double):SynapseData = SynapseData(neuronId, SynapseWeight(weight))

  def fromJson(jsonStr: String): SynapseData = read[SynapseData](jsonStr)

  implicit def fromDouble(weight: Double):SynapseWeight = SynapseWeight(weight)
}