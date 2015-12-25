package anna.epengine

import anna.logger.LOG
import anna.logger.LOG.error
import anna.utils.RandomNumber

class Probability private (val value: Double) extends AnyVal {
  def +(other: Probability) = Probability(value + other.value)
  def -(other: Probability) = Probability(value - other.value)

  def toss = RandomNumber() <= value
  
  def full = value == Probability.FULL
  def void = value == Probability.VOID
}

object Probability{
  val FULL = 1.0
  val VOID = 0.0
  
  def apply(d: Double) = fromDouble(d)

  implicit def fromDouble(d: Double):Probability =
    if(d < VOID && d > VOID - 0.001){
      error(this,s"Trying to set the probability to $d - assuming a floating point error and using $VOID")
      new Probability(VOID)
    } else if(d > FULL && d < FULL + 0.001){
      error(this,s"Trying to set the probability to $d - assuming a floating point error and using $FULL")
      new Probability(FULL)
    } else {
      assert(d >= VOID && d <= FULL, s"You can't set the probability outside the range <$VOID,$FULL>: $d")
      new Probability(d)
    }

  
  implicit def toDouble(p: Probability):Double = p.value

  def normalize(probs: Probability*):List[Probability] = {
    val sum = probs.map(_.value).sum
    if(sum > VOID) probs.map( p => Probability(p / sum) ).toList
    else Probability(FULL) :: probs.tail.map( _ => Probability(VOID) ).toList
  }
  
  def normalize[T](tuples: List[(Probability, T)]): List[(Probability, T)] = 
    normalize(tuples.map(_._1): _*).zip(tuples.map(_._2))

  def normalize[T](map: Map[Probability, T]): List[(Probability, T)] =
    normalize(map.keys.toSeq: _*).zip(map.values)
  
  def performRandom(tuples: (Probability,()=>Unit)*): Unit = performRandom(tuples.toList)

  def performRandom(list: List[(Probability,()=>Unit)]): Unit = RandomNumber() match {
    case _ if list.isEmpty => LOG.error(this, "performRandom on an empty list")
    case VOID if list.map(_._1.value).sum == VOID => (list.head._2)()
    case VOID => (list.collectFirst({ case (p, f) if p > VOID => f }).get)()
    case r => performRandom(r, list)
  }

  private def performRandom(r: Double, tuples: List[(Probability,()=>Unit)]): Unit = {
    val list = normalize(tuples)
    var index = 0
    var d = r
    while (d >= VOID) {
      d -= list(index)._1
      if (d < VOID) list(index)._2() else index += 1
    }
  }
}
