package anna.utils

import anna.utils.Utils.{formats, classTagToManifest}
import org.json4s.native.Serialization.{read, writePretty}
import scala.reflect.ClassTag

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
  given Conversion[Range, IntRange] = r => IntRange(r.start, r.end)
  def fromJson(str: String)(using ct: ClassTag[IntRange]): IntRange = read[IntRange](str)
}