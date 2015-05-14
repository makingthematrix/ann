package anna.utils

import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

/**
 * Created by gorywoda on 12.01.15.
 */

case class IntRange(start: Int, end: Int)  {
  def choose(x: Double) = math.round(x *(end - r.start - 1) + r.start).toInt
  def r = start to end
  def contains(x: Int) = r.contains(x)
  def toJson = writePretty(this)
}

object IntRange {
  implicit def fromRange(r: Range):IntRange = IntRange(r.start, r.end)
  def fromJson(str: String) = read[IntRange](str)
}