package anna.async

import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

sealed trait NeuronType extends Any

final case class NeuronTypeStandard() extends NeuronType {
  def toJson: String = writePretty(this)
}

final case class NeuronTypeDummy() extends NeuronType {
  def toJson: String = writePretty(this)
}

final case class NeuronTypeSilencing() extends NeuronType {
  def toJson: String = writePretty(this)
}

object NeuronType {
  def parse(str: String): NeuronType = str match {
    case "STANDARD" => NeuronTypeStandard()
    case "DUMMY" => NeuronTypeDummy()
    case "SILENCING" => NeuronTypeSilencing()
  }

  def fromJson(str: String): NeuronType = read[NeuronType](str)
}

