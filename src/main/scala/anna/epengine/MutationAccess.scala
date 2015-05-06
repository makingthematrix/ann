package anna.epengine

import org.json4s.native.Serialization.{ read, writePretty }
import anna.utils.Utils.formats

/**
 * Created by gorywoda on 08.03.15.
 */

sealed trait MutationAccess extends Any

case class MutationAccessFull() extends MutationAccess {
  def toJson = writePretty(this)
}
case class MutationAccessDontDelete() extends MutationAccess {
  def toJson = writePretty(this)
}
case class MutationAccessDontMutate() extends MutationAccess {
  def toJson = writePretty(this)
}

object MutationAccess {
  def parse(str: String) = str match {
    case "FULL" => MutationAccessFull()
    case "DONTDELETE" => MutationAccessDontDelete()
    case "DONTMUTATE" => MutationAccessDontMutate()
  }

  def fromJson(str: String) = read[MutationAccess](str)
}
