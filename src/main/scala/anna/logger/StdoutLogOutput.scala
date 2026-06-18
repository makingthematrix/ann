package anna.logger

final class StdoutLogOutput extends LogOutput {
  override def id: String = "stdout"
  override def println(x: String): Unit = Console.println(x)
  override def log: String = ""
  override def close(): Unit = {}
}