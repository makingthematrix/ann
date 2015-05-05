package anna.data

import anna.utils.DoubleRange._

/**
 * Created by gorywoda on 05.05.15.
 */
sealed trait SynapseTrait extends Any
case class SynapseWeight(weight: Double) extends AnyVal with SynapseTrait {
  def check = assert((-1.0<=>1.0).contains(weight))

  override def toString = weight.toString
}
case object Hush extends SynapseTrait {
  override def toString = "Hush"
}

object SynapseTrait {
  def apply(str: String) = str match {
    case "Hush" => Hush
    case str => SynapseWeight(str.toDouble)
  }
}
