package anna.data

import anna.utils.DoubleRange.given
import scala.language.implicitConversions
import anna.utils.Utils.{formats, classTagToManifest}
import org.json4s.native.Serialization.{read, writePretty}
import scala.reflect.ClassTag

/**
 * Created by gorywoda on 05.05.15.
 */
sealed trait SynapseTrait

final case class SynapseWeight(weight: Double) extends SynapseTrait {
  def check(): Unit = assert((-1.0<=>1.0).contains(weight))
  def toJson: String = writePretty(this)
}

final case class Silence() extends SynapseTrait {
  def toJson: String = writePretty(this)
}

final case class Wake() extends SynapseTrait {
  def toJson: String = writePretty(this)
}

object SynapseTrait {
  def apply(str: String): SynapseTrait = str match {
    case "Silence" => Silence()
    case "Wake" => Wake()
    case str => SynapseWeight(str.toDouble)
  }

  def fromJson(str: String)(using ct: ClassTag[SynapseTrait]): SynapseTrait = read[SynapseTrait](str)
}
