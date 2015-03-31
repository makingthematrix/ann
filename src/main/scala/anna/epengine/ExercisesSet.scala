package anna.epengine

import anna.Context
import anna.utils.Utils
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._

/**
 * Created by gorywoda on 11.03.15.
 */
case class ExercisesSet(name: String, exerciseNames: Set[String]){
  def exercises = exerciseNames.map( exName => ExercisesLibrary().apply(exName) ).toList
  def validate = exerciseNames.foreach( exName =>
    assert(ExercisesLibrary().get(exName) != None, s"Exercise not defined in the set $name: $exName")
  )

  def resultForAllPassed =
    exerciseNames.toList.map(exName => ExercisesLibrary().get(exName).map(_.success).getOrElse(0.0)).sum

  def save = Utils.save(Context().exercisesSetDir + "/" + name + ".json",toJson)

  def toJson = compact(toRawJson)
  def toPrettyJson = pretty(toRawJson)

  private lazy val toRawJson = render(("name" -> name) ~ ("exerciseNames" -> exerciseNames))
}

object ExercisesSet {
  implicit val formats = org.json4s.DefaultFormats

  def fromJson(jsonStr: String):ExercisesSet = parse(jsonStr).extract[ExercisesSet]
  def load(name: String) = ExercisesSet.fromJson(Utils.load(Context().exercisesSetDir + "/" + name + ".json"))
}
