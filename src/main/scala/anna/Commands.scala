package anna

import anna.data.NetData
import anna.epengine.{NetGenome, Engine, ExercisesSet}
import anna.utils.Utils
import anna.logger.LOG._

/**
 * Created by gorywoda on 06.06.15.
 */
object Commands {

  def listEvolutions = Utils.listDirs(Context().evolutionDir).foreach(println)

  def listSets = Utils.listDirs(Context().exercisesSetDir).foreach(println)

  def set(name: String):ExercisesSet = ExercisesSet.load(name)

  private var engineOpt:Option[Engine] = None

  def create(name: String, inputIds: List[String], outputIds: List[String], template: NetData, setName: String):Unit = {
    engineOpt = Some(Engine(name, inputIds, outputIds, template, set(setName)))
    println("done")
  }

  def open(name: String):Unit = {
    engineOpt = Some(Engine(name))
  }

  def run(iterations: Int):Unit = engineOpt match {
    case Some(engine) => engine.run(iterations)
    case None => exception(this, "Unable to run as no engine is ready")
  }

  def best:NetGenome = engineOpt match {
    case Some(engine) => engine.best
    case None => throw new IllegalArgumentException("Unable to get the best genome as no engine is ready")
  }

}
