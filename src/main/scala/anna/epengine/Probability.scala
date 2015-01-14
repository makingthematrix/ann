package anna.epengine

case class Probability(value: Double) extends AnyVal {
  def +(other: Probability) = Probability(value + other.value)
  def -(other: Probability) = Probability(value - other.value)
}

object Probability{
  implicit def fromDouble(d: Double):Probability = {
    assert(d >= 0.0 && d <= 1.0, s"You can't set the probability outside the range <0.0,1.0>: $d")
    new Probability(d)
  }
  
  implicit def toDouble(p: Probability):Double = p.value

  def normalize(probs: Probability*) = {
    val sum:Double = probs.sum
    probs.map( _ / sum ).toList
  }

  // @todo: This is rather low level. I'd prefer something returning an enum.
  def chooseOne(probs: Probability*):Int = {
    val list = normalize(probs: _*)
    val r:Double = RandomNumber()

    var d = 0.0
    var index = 0
    while(d < r){
      d += list(index)
      index += 1
    }
    index
  }
}
