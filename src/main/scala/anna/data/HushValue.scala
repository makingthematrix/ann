package anna.data

/**
 * Created by gorywoda on 05.05.15.
 */
case class HushValue(iterations: Int = 1) extends AnyVal

object HushValue {
  def apply(str:String):HushValue = HushValue(str.toInt)
}
