package anna.data

/**
 * Created by gorywoda on 05.05.15.
 */
import org.json4s.native.Serialization.{ read, writePretty }
import anna.utils.Utils.formats

case class HushValue(iterations: Int = 1){
  def toJson = writePretty(this)
}

object HushValue {
  def apply(str:String):HushValue = HushValue(str.toInt)
  def fromJson(str: String) = read[HushValue](str)
}
