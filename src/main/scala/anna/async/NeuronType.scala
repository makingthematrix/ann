package anna.async

import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

sealed trait NeuronType extends Any

case class NeuronTypeStandard() extends NeuronType {
  def toJson = writePretty(this)
}

case class NeuronTypeDummy() extends NeuronType {
  def toJson = writePretty(this)
}

case class NeuronTypeHush() extends NeuronType {
  def toJson = writePretty(this)
}

object NeuronType {
  def parse(str: String) = str match {
    case "STANDARD" => NeuronTypeStandard()
    case "DUMMY" => NeuronTypeDummy()
    case "HUSH" => NeuronTypeHush()
  }

  def fromJson(str: String) = read[NeuronType](str)
}

