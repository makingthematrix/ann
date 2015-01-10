package anna.utils

case class RichD(val d: Double) extends AnyVal {
  def <=>(other: Double) = DoubleRange(d, other)
}

case class DoubleRange(from: Double, to: Double){
  def iterator(resolution: Int) = (0 to resolution).map{ x => choose(x.toDouble/resolution) }.view.iterator
  def choose(x: Double) = x*(to-from)+from
  def contains(x: Double):Boolean = x >= from && x <= to
  def contains(r: DoubleRange):Boolean = contains(r.from) && contains(r.to)
  def intersects(r: DoubleRange) = !contains(r) && !r.contains(this)
  def univalue: Option[Double] = if(from == to) Some(from) else None
}

object DoubleRange {  
  implicit def fromDouble(d: Double) = new RichD(d)
}