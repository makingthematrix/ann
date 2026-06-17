package anna.utils

import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

import scala.language.implicitConversions

/**
 * Created by gorywoda on 12.01.15.
 */

final case class IntRange(start: Int, end: Int)  {
  def choose(x: Double): Int = math.round(x *(end - r.start - 1) + r.start).toInt
  def r: Range = start to end
  def contains(x: Int): Boolean = r.contains(x)
  def toJson: String = writePretty(this)
}

object IntRange {
  implicit def fromRange(r: Range): IntRange = IntRange(r.start, r.end)
  def fromJson(str: String): IntRange = read[IntRange](str)
}