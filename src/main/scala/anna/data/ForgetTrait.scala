package anna.data

/**
 * Created by gorywoda on 05.05.15.
 */
sealed trait ForgetTrait extends Any

case class ForgetValue(value: Double) extends AnyVal with ForgetTrait {
  override def toString = value.toString
}

case object ForgetAll extends ForgetTrait {
  override def toString = "ForgetAll"
}
case object DontForget extends ForgetTrait {
  override def toString = "DontForget"
}

object ForgetTrait {
  def apply(str: String) = str match {
    case "DontForget" => DontForget
    case "ForgetAll" => ForgetAll
    case str => ForgetValue(str.toDouble)
  }
}
