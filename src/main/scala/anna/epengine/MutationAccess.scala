package anna.epengine

/**
 * Created by gorywoda on 08.03.15.
 */
object MutationAccess extends Enumeration {
  type MutationAccess = Value
  val FULL, DONTDELETE, DONTMUTATE = Value
}
