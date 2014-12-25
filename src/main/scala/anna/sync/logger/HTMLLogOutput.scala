package anna.sync.logger

class HTMLLogOutput(override val id: String) extends StringLogOutput(id){
	override def println(str: String) = super.println(str + "<br>")
}