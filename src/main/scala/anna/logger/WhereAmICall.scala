package anna.logger

import java.io.{PrintWriter, StringWriter}

class WhereAmICall extends IllegalArgumentException("Where Am I?")

object WhereAmICall{
  def whereAmI: String = {
    val aThrowable = new WhereAmICall()
	  val result = new StringWriter()
	  aThrowable.printStackTrace(new PrintWriter(result))
	  result.toString
  }
}