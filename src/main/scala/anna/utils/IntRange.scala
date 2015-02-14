package anna.utils

/**
 * Created by gorywoda on 12.01.15.
 */
case class IntRange(r: Range) extends AnyVal {
  def choose(x: Double):Int = {
    val end = if(r.isInclusive) r.end else r.end - 1
    math.round(x *(end - r.start) + r.start).toInt
  }
}

object IntRange {
  implicit def fromRange(r: Range):IntRange = IntRange(r)
}