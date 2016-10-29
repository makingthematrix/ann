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

case class NeuronTypeSilencing() extends NeuronType {
  def toJson = writePretty(this)
}

object NeuronType {
  def parse(str: String) = str match {
    case "STANDARD" => NeuronTypeStandard()
    case "DUMMY" => NeuronTypeDummy()
    case "SILENCING" => NeuronTypeSilencing()
  }

  def fromJson(str: String) = read[NeuronType](str)
}

