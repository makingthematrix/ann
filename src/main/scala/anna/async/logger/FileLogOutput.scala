package anna.async.logger

import java.io.{BufferedWriter, File, FileWriter, IOException}
 
class FileLogOutput(val fileName: String) extends LogOutput {
  private var writer:Option[BufferedWriter] = None
  
  private def newWriter = {
    val file = new File(fileName)
	if (!file.exists()) {
	  file.createNewFile()
	  file.setReadable(true)
	  file.setWritable(true)
	}
	new BufferedWriter(new FileWriter(file, true))
  }
  
  private def getWriter = writer match {
    case Some(w) => w
    case None => {
      val w = newWriter
      writer = Some(w)
      w
    }
  }
  
  override def println(str: String) = try {
    val w = getWriter
    w.write(str)
	w.newLine()
	w.flush()
  } catch {
	case ex: IOException => ex.printStackTrace()
  }	
  
  override def log: String = ""

  override def close(): Unit = writer match {
    case Some(w) => try {
      w.close()
      writer = None
    } catch {
      case ex: IOException => ex.printStackTrace()
    }
    case None =>
  }
	
  override def id: String = fileName

}