package anna.utils

import scala.util.Random

/**
 * Created by gorywoda on 12.01.15.
 */
object RandomNumber {
  private var _rand: Option[Random] = None
  private def rand() = _rand match {
    case None =>
      val r = new Random()
      _rand = Some(r)
      r
    case Some(r) => r
  }

  def apply():Double = rand().nextDouble()
  def apply(start:Int, end: Int):Int = apply(IntRange(start until end))
  def apply(range: IntRange):Int = range.choose(apply())
  def apply(range: DoubleRange):Double = range.choose(apply())
  def apply(end: Int):Int = apply(0, end)
  def apply[T](it: Iterable[T]):T = it.drop(apply(it.size)).head
}
