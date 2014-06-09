package main.logger

class StringLogOutput(val id: String) extends LogOutput {
  private val sb = StringBuilder.newBuilder
  override def println(x: String) = sb.append(timeTag).append('>').append(x).append('\n')
  override def log = sb.toString
  override def close() = sb.setLength(0)
}