package anna.logger

final class HTMLLogOutput(override val id: String) extends StringLogOutput(id){
	override def println(str: String): Unit = super.println(str + "<br>")
}