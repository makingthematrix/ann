package anna.epengine

import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

/**
 * Created by gorywoda on 08.03.15.
 */

sealed trait MutationAccess extends Any

case class MutationAccessFull() extends MutationAccess {
  def toJson = writePretty(this)
}
case class MutationAccessInput() extends MutationAccess {
  def toJson = writePretty(this)
}
case class MutationAccessOutput() extends MutationAccess {
  def toJson = writePretty(this)
}

object MutationAccess {
  def parse(str: String) = str match {
    case "FULL" => MutationAccessFull() // can delete, can mutate, can connect to
    case "INPUT" => MutationAccessInput() // cannot delete, cannot mutate, cannot connect to
    case "OUTPUT" => MutationAccessOutput() // cannot delete, can mutate, can connect to
  }

  def fromJson(str: String) = read[MutationAccess](str)
}
