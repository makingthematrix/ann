package anna.logger

import java.util.Calendar

trait LogOutput {
	protected def timeTag: String = Calendar.getInstance.toString

	def log: String

	def println(str: String): Unit
	
	def close(): Unit
	
	def id: String
}