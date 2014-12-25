package anna.async.logger

class SystemLogOutput extends LogOutput {
  override def log = ""

  override def println(str: String) = System.console().printf(str+'\n')
  
  override def close(){
    // nothing to do here
  }
  
  override def id = "stdout"
}