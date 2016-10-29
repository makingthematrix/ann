package anna.data

/**
 * Created by gorywoda on 05.05.15.
 */
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

case class SilenceIterations(iterations: Int = 1){ // @todo: why not simply Int?
  def toJson = writePretty(this)
}

object SilenceIterations {
  def apply(str:String):SilenceIterations = SilenceIterations(str.toInt)
  def fromJson(str: String) = read[SilenceIterations](str)
}
