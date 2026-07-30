package anna.utils

import anna.utils.Utils.{formats, classTagToManifest}
import org.json4s.native.Serialization.{read, writePretty}
import scala.reflect.ClassTag

import scala.annotation.tailrec
import scala.language.implicitConversions

final case class RichD(d: Double) extends AnyVal {
  def <=>(other: Double): DoubleRange = DoubleRange(d, other)
}

final case class DoubleRange(from: Double, to: Double) {
  def iterator(resolution: Int): Iterator[Double] = (0 to resolution).map{ x => choose(x.toDouble/resolution) }.view.iterator

  @tailrec
  def choose(x: Double, withoutValues: List[Double] = Nil): Double = {
    val result = x*(to-from)+from
    if(withoutValues.contains(result)) choose(x, withoutValues) else result
  }

  def choose(x: Double, withoutValue: Double): Double = choose(x, List(withoutValue))

  def contains(x: Double): Boolean = x >= from && x <= to
  def contains(r: DoubleRange): Boolean = contains(r.from) && contains(r.to)
  def intersects(r: DoubleRange): Boolean = !contains(r) && !r.contains(this)
  def univalue: Option[Double] = if(from == to) Some(from) else None

  def toJson: String = writePretty(this)
}

object DoubleRange {  
  given Conversion[Double, RichD] = d => RichD(d)
  def fromJson(jsonStr: String)(using ct: ClassTag[DoubleRange]): DoubleRange = read[DoubleRange](jsonStr)
}