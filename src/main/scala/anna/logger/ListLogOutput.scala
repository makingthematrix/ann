package anna.logger

/**
 * Created by gorywoda on 07.05.15.
 */

import scala.collection.mutable

class ListLogOutput(val id: String) extends LogOutput {
  private val _list = mutable.ListBuffer[String]()
  override def println(x: String) = _list += x
  override def log = _list.mkString("\n")
  override def close() = _list.clear()

  def list = _list.toList
}
