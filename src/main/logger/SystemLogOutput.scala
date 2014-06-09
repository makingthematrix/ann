package main.logger

class SystemLogOutput extends LogOutput {
  override def log = ""

  override def println(str: String) = println(str)
  
  override def close(){
    // nothing to do here
  }
  
  override def id = "stdout"
}