package anna.async

sealed trait NeuronType extends Any

case class NeuronTypeStandard() extends NeuronType
case class NeuronTypeDummy() extends NeuronType
case class NeuronTypeHush() extends NeuronType

object NeuronType {
  def parse(str: String) = str match {
    case "STANDARD" => NeuronTypeStandard()
    case "DUMMY" => NeuronTypeDummy()
    case "HUSH" => NeuronTypeHush()
  }
}

