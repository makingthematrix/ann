package anna.logger

final class SystemLogOutput extends LogOutput {
  override def log: String = ""

  override def println(str: String): Unit = System.console().printf(str+'\n')
  
  override def close(): Unit = {
    // nothing to do here
  }
  
  override def id = "stdout"
}