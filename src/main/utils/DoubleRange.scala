package main.utils

case class DoubleRange(from: Double, to: Double){
  def iterator(resolution: Int) = (0 to resolution).map{ x => choose(x.toDouble/resolution) }.view.iterator
  def choose(x: Double) = x*(to-from)+from
}

case class RichD(val d: Double) extends AnyVal {
  def <=>(other: Double) = DoubleRange(d, other)
}

object DoubleRange {  
  implicit def fromDouble(d: Double) = new RichD(d)
}