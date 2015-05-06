package anna.epengine

/**
 * Created by gorywoda on 08.03.15.
 */

sealed trait MutationAccess extends Any

case class MutationAccessFull() extends MutationAccess
case class MutationAccessDontDelete() extends MutationAccess
case class MutationAccessDontMutate() extends MutationAccess

object MutationAccess {
  def parse(str: String) = str match {
    case "FULL" => MutationAccessFull()
    case "DONTDELETE" => MutationAccessDontDelete()
    case "DONTMUTATE" => MutationAccessDontMutate()
  }
}
