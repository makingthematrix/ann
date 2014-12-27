package anna.async

object NeuronType extends Enumeration {
  type NeuronType = Value
  val STANDARD, DUMMY, HUSH = Value

  def parse(str: String) = str match {
    case "STANDARD" => STANDARD
    case "DUMMY" => DUMMY
    case "HUSH" => HUSH
  }
}

