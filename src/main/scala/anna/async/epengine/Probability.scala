package anna.async.epengine

case class Probability(value: Double) extends AnyVal 

object Probability{
  implicit def fromDouble(d: Double):Probability = {
    assert(d >= 0.0 && d <= 1.0, s"You can't set the probability outside the range <0.0,1.0>: $d")
    new Probability(d)
  }
  
  implicit def toDouble(p: Probability):Double = p.value
}
