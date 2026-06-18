package anna.logger

class StringLogOutput(val id: String) extends LogOutput {
  private val sb = new StringBuilder()
  override def println(x: String): Unit = sb.append(x).append('\n')
  override def log: String = sb.toString
  override def close(): Unit = sb.setLength(0)
}