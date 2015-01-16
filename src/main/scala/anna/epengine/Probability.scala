package anna.epengine

import anna.logger.LOG._

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
    val sum = probs.map(_.value).sum
    probs.map( _ / sum ).toList
  }

  // @todo: This is rather low level. I'd prefer something returning an enum.
  def chooseOne(probs: Probability*):Int = {
    val list = normalize(probs: _*)
    val r = RandomNumber()

    var dSum = 0.0
    var index = 0
    while(dSum < r){
      if(dSum + list(index) < r) index += 1
      dSum += list(index)
    }
    index
  }
}
