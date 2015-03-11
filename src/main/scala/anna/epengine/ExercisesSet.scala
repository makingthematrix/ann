package anna.epengine

import anna.async.NeuronType
import anna.data.{SynapseData, NeuronData}
import anna.logger.LOG._
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._

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
  def apply(name: String, exerciseNames: Set[String]):ExercisesSet = new ExercisesSet(name, exerciseNames)
  def apply(name: String, exerciseNames: List[String]):ExercisesSet = new ExercisesSet(name, exerciseNames.toSet)

  def fromJson(jsonStr: String):ExercisesSet = parse(jsonStr).extract[ExercisesSet]
}
