package anna.utils

import scala.annotation.tailrec

case class RichD(val d: Double) extends AnyVal {
  def <=>(other: Double) = DoubleRange(d, other)
}

case class DoubleRange(from: Double, to: Double){
  def iterator(resolution: Int) = (0 to resolution).map{ x => choose(x.toDouble/resolution) }.view.iterator

  @tailrec
  final def choose(x: Double, withoutValues: List[Double] = Nil):Double = {
    val result = x*(to-from)+from
    if(withoutValues.contains(result)) choose(x, withoutValues) else result
  }
  def choose(x: Double, withoutValue: Double):Double = choose(x, List(withoutValue))

  def contains(x: Double):Boolean = x >= from && x <= to
  def contains(r: DoubleRange):Boolean = contains(r.from) && contains(r.to)
  def intersects(r: DoubleRange) = !contains(r) && !r.contains(this)
  def univalue: Option[Double] = if(from == to) Some(from) else None
}

object DoubleRange {  
  implicit def fromDouble(d: Double) = new RichD(d)
}