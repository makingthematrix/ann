package anna.async

import anna.utils.Utils.{formats, classTagToManifest}
import org.json4s.native.Serialization.{read, writePretty}
import scala.reflect.ClassTag

enum NeuronType {
  case NeuronTypeStandard
  case NeuronTypeDummy
  case NeuronTypeSilencing

  def toJson: String = writePretty(this)
}

object NeuronType {
  def fromJson(str: String)(using ct: ClassTag[NeuronType]): NeuronType = read[NeuronType](str)
}

