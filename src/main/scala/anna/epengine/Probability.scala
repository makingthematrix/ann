package anna.epengine

import anna.logger.LOG.error
import anna.utils.RandomNumber

class Probability private (val value: Double) extends AnyVal {
  def +(other: Probability) = Probability(value + other.value)
  def -(other: Probability) = Probability(value - other.value)

  def toss = RandomNumber() <= value
}

object Probability{
  def apply(d: Double) = fromDouble(d)

  implicit def fromDouble(d: Double):Probability = {
    val dd = if(d < 0.0 && d > -0.001){
      error(this,s"Trying to set the probability to $d - assuming a floating point error and using 0.0")
      0.0
    } else if(d > 1.0 && d < 1.001){
      error(this,s"Trying to set the probability to $d - assuming a floating point error and using 1.0")
      1.0
    } else {
      assert(d >= 0.0 && d <= 1.0, s"You can't set the probability outside the range <0.0,1.0>: $d")
      d
    }
    new Probability(dd)
  }
  
  implicit def toDouble(p: Probability):Double = p.value

  def normalize(probs: Probability*):List[Probability] = {
    val sum = probs.map(_.value).sum
    if(sum > 0.0) probs.map( p => Probability(p / sum) ).toList
    else Probability(1.0) :: probs.tail.map( _ => Probability(0.0) ).toList
  }
  
  def normalize[T](tuples: List[(Probability, T)]): List[(Probability, T)] = 
    normalize(tuples.map(_._1): _*).zip(tuples.map(_._2)).toList

  def normalize[T](map: Map[Probability, T]): List[(Probability, T)] =
    normalize(map.keys.toSeq: _*).zip(map.values).toList
  
  def performRandom(tuples: (Probability,()=>Unit)*): Unit = RandomNumber() match {
    case 0.0 if tuples.map(_._1.value).sum == 0.0 => (tuples.head._2)()
    case 0.0 => (tuples.collectFirst({ case (p, f) if p > 0.0 => f }).get)()
    case r => performRandom(r, tuples.toList)
  }

  private def performRandom(r: Double, tuples: List[(Probability,()=>Unit)]): Unit = {
    val list = normalize(tuples)
    var index = 0
    var d = r
    while (d >= 0.0) {
      d -= list(index)._1
      if (d < 0.0) list(index)._2() else index += 1
    }
  }
}
