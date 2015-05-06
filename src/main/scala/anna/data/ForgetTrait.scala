package anna.data

import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

/**
 * Created by gorywoda on 05.05.15.
 */
sealed trait ForgetTrait extends Any

case class ForgetValue(value: Double) extends ForgetTrait {
  def toJson = writePretty(this)
}

case class ForgetAll() extends ForgetTrait {
  def toJson = writePretty(this)
}

case class DontForget() extends ForgetTrait {
  def toJson = writePretty(this)
}

object ForgetTrait {
  def apply(str: String) = str match {
    case "DontForget" => DontForget()
    case "ForgetAll" => ForgetAll()
    case str => ForgetValue(str.toDouble)
  }

  def fromJson(str: String) = read[ForgetTrait](str)
}
