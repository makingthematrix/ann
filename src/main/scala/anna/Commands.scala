package anna

import anna.async.NetBuilder
import anna.data.NetData
import anna.epengine.{NetGenome, Engine, ExercisesSet}
import anna.utils.Utils
import anna.logger.LOG._
import anna.async.NetBuilderOps._

/**
 * Created by gorywoda on 06.06.15.
 */
object Commands {

  def listEvolutions = {
    val dirs = Utils.listDirs(Context().evolutionDir)
    println(s"the evolutions directory is ${Context().evolutionDir}")
    println(s"evolutions found: ${dirs.size}")
    dirs.foreach( println )
  }

  def listSets = {
    val sets = Utils.listFiles(Context().exercisesSetDir)
    println(s"the exercises sets directory is ${Context().exercisesSetDir}")
    println(s"sets found: ${sets.size}")
    sets.map(s => s.substring(0,s.lastIndexOf(".json"))).foreach( println )
  }

  private var exercisesSetOpt:Option[ExercisesSet] = None

  def set(name: String):Unit = {
    exercisesSetOpt = Some(ExercisesSet.load(name))
  }

  def set:String = exercisesSetOpt.map(_.name).getOrElse("The exercises set is not set")

  private var engineOpt:Option[Engine] = None

  val dotLineTemplate =
    NetBuilder().addInput("in").chain("mi11",1.0,0.5).chain("mi12",1.0,0.5).chain("dot",1.0,0.5)
    .use("in").chain("mi21",1.0,0.5).chain("mi22",1.0,0.5).chain("line",1.0,0.5)
    .use("mi12").hush("mi21")
    .use("mi21").hush("mi11")
    .use("dot").hush("line")
    .use("line").hush("dot")
    .setName("net").data

  val simplestTemplate = NetBuilder().addInput("in").chain("dot",1.0,0.5).use("in").chain("line",1.0,0.5).setName("net").data

  private var inputIdsOpt:Option[List[String]] = None

  def inputIds(ids:String*): Unit ={
    inputIdsOpt = Some(ids.toList)
  }

  def inputIds:String = inputIdsOpt.map(_.mkString(", ")).getOrElse("Input ids not set")

  private var outputIdsOpt:Option[List[String]] = None

  def outputIds(ids:String*): Unit ={
    outputIdsOpt = Some(ids.toList)
  }

  def outputIds:String = outputIdsOpt.map(_.mkString(", ")).getOrElse("Output ids not set")

  def create(name: String, template: NetData):Unit = {
    val inputIds:List[String] = inputIdsOpt.getOrElse(throw new IllegalArgumentException("No input ids set"))
    val outputIds:List[String] = outputIdsOpt.getOrElse(throw new IllegalArgumentException("No output ids set"))
    val exercisesSet = exercisesSetOpt.getOrElse(throw new IllegalArgumentException("No exercises set... set"))
    engineOpt = Some(Engine(name, inputIds, outputIds, template, exercisesSet))
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
