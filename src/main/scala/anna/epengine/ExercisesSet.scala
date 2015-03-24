package anna.epengine

import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.DefaultFormats

/**
 * Created by gorywoda on 11.03.15.
 */
case class ExercisesSet(name: String, exerciseNames: Set[String]){
  def exercises = exerciseNames.map( exName => ExerciseLibrary().apply(exName) ).toList
  def validate = exerciseNames.foreach( exName =>
    assert(ExerciseLibrary().get(exName) != None, s"Exercise not defined in the set $name: $exName")
  )

  def toJson = compact(toRawJson)
  def toPrettyJson = pretty(toRawJson)

  private lazy val toRawJson = render(("name" -> name) ~ ("exerciseNames" -> exerciseNames))
}

object ExercisesSet {
  def apply(name: String, exerciseNames: List[String]):ExercisesSet = new ExercisesSet(name, exerciseNames.toSet)

  implicit val formats = org.json4s.DefaultFormats

  def fromJson(jsonStr: String):ExercisesSet = parse(jsonStr).extract[ExercisesSet]
}
