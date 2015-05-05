package anna.data

import anna.utils.DoubleRange._

import org.json4s.native.Serialization.{ read, writePretty }
import anna.utils.Utils.formats

/**
 * Created by gorywoda on 05.05.15.
 */
sealed trait SynapseTrait extends Any

case class SynapseWeight(weight: Double) extends SynapseTrait {
  def check = assert((-1.0<=>1.0).contains(weight))
  def toJson = writePretty(this)
}

case class Hush() extends SynapseTrait {
  def toJson = writePretty(this)
}

object SynapseTrait {
  def apply(str: String) = str match {
    case "Hush" => Hush()
    case str => SynapseWeight(str.toDouble)
  }

  def fromJson(str: String) = read[SynapseTrait](str)
}
