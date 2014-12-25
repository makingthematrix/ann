package anna.sync.logger

class StdoutLogOutput extends LogOutput {
  override def id = "stdout"
  override def println(x: String) = Console.println(x)
  override def log = ""
  override def close(){}
}