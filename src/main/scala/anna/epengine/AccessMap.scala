package anna.epengine

/**
 * Created by gorywoda on 12/13/15.
 */
object AccessMap {
  def apply(inputs: List[String], outputs: List[String]):Map[String,MutationAccess] =
    inputs.map(_ -> MutationAccessInput()).toMap ++ outputs.map(_ -> MutationAccessOutput())
}
