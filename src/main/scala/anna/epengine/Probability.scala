package anna.epengine

class Probability private (val value: Double) extends AnyVal {
  def +(other: Probability) = Probability(value + other.value)
  def -(other: Probability) = Probability(value - other.value)
}

object Probability{
  def apply(d: Double) = fromDouble(d)

  implicit def fromDouble(d: Double):Probability = {
    assert(d >= 0.0 && d <= 1.0, s"You can't set the probability outside the range <0.0,1.0>: $d")
    new Probability(d)
  }
  
  implicit def toDouble(p: Probability):Double = p.value

  def normalize(probs: Probability*) = {
    val sum = probs.map(_.value).sum
    if(sum > 0.0) probs.map( p => Probability(p / sum) ).toList
    else Probability(1.0) :: probs.tail.map( _ => Probability(0.0) ).toList
  }

  def performRandom(tuples: (Probability,()=>Unit)*): Unit = RandomNumber() match {
    case 0.0 if tuples.map(_._1.value).sum == 0.0 => (tuples.head._2)()
    case 0.0 => (tuples.collectFirst({ case (p, f) if p > 0.0 => f }).get)()
    case r => performRandom(r, tuples.toList)
  }

  private def performRandom(r: Double, tuples:List[(Probability,()=>Unit)]): Unit = {
    val list = normalize(tuples.map(_._1): _*).zip(tuples.map(_._2)).toList
    var index = 0
    var d = r
    while (d >= 0.0) {
      d -= list(index)._1
      if (d < 0.0) list(index)._2() else index += 1
    }
  }
}
