package anna.epengine

import anna.utils.IntRange

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
  def apply(start:Int, end: Int):Int = IntRange(start until end).choose(apply())
  def apply(end: Int):Int = apply(0, end)
}
