package main.async.logger

import java.util.Calendar

trait LogOutput {
	protected def timeTag = Calendar.getInstance().toString()

	def log: String

	def println(str: String): Unit
	
	def close(): Unit
	
	def id: String
}