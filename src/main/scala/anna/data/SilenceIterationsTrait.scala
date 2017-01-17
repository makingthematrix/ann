package anna.data

import anna.utils.Utils.formats
import org.json4s.native.Serialization.writePretty

/**
  * Created by gorywoda on 1/17/17.
  */
sealed trait SilenceIterationsTrait extends Any

case class SilenceIterations(iterations: Int) extends SilenceIterationsTrait {
  def check = assert(iterations >= 0)
  def toJson = writePretty(this)
}

case class SilenceForever() extends SilenceIterationsTrait {
  def toJson = writePretty(this)
}