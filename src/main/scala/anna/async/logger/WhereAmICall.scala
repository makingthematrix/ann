package anna.async.logger

import java.io.{PrintWriter, StringWriter}

class WhereAmICall extends IllegalArgumentException("Where Am I?")

object WhereAmICall{
  def whereAmI = {
    val aThrowable = new WhereAmICall()
	val result = new StringWriter()
	aThrowable.printStackTrace(new PrintWriter(result))
	result.toString()
  }
}