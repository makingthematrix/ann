package anna.logger

class StringLogOutput(val id: String) extends LogOutput {
  private val sb = StringBuilder.newBuilder
  override def println(x: String) = sb.append(x).append('\n')
  override def log = sb.toString
  override def close() = sb.setLength(0)
}