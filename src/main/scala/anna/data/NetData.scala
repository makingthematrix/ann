package anna.data

import anna.logger.LOG._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import org.json4s._

/**
 * Created by gorywoda on 03.01.15.
 */
case class NetData(id: String, neurons: List[NeuronData], inputs: List[String]){
  def toJson = compact(toRawJson)
  def toPrettyJson = pretty(toRawJson) // for debugging purposes only

  def withId(id: String) = NetData(id, neurons, inputs)
  def withNeurons(neurons: List[NeuronData]) = NetData(id, neurons, inputs)
  def withInputs(inputs: List[String]) = NetData(id, neurons, inputs)

  private def toRawJson = {
    val neuronsJson = neurons.map{ _.toJson }
    val json = ("id" -> id) ~
      ("neurons" -> neuronsJson) ~
      ("inputs" -> inputs)
    render(json)
  }
}

object NetData {
  def apply(id: String):NetData = NetData(id, Nil, Nil)

  def fromJson(jsonStr: String):NetData = {
    val json = parse(jsonStr)
    val parsed:List[NetData] = for {
      JObject(data) <- json
      JField("id", JString(id)) <- data
      JField("neurons", JArray(neuronsJson)) <- data
      JField("inputs", JArray(inputsJson)) <- data
    } yield NetData(id, parseNeurons(neuronsJson), parseInputs(inputsJson))

    if(parsed.size != 1) exception(this, s"Unable to parse JSON: $jsonStr")
    parsed(0)
  }

  private def parseNeurons(neuronsJson: List[JValue]) = neuronsJson.map {
    case JString(n) => NeuronData.fromJson(n)
    case _ => throw new IllegalArgumentException(s"Unable to parse JSON $neuronsJson")
  }

  private def parseInputs(inputsJson: List[JValue]) = inputsJson.map {
    case JString(in) => in
    case _ => throw new IllegalArgumentException(s"Unable to parse JSON $inputsJson")
  }
}
