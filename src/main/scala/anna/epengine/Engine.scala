package anna.epengine

class Engine {

}

object Engine {
  private var instance:Option[Engine] = None

  def apply():Engine = instance match {
    case None => val engine = new Engine(); instance = Some(engine); engine
    case Some(engine) => engine
  }
}