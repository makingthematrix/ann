package anna.data

import anna.Context
import anna.logger.LOG._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import org.json4s._

/**
 * Created by gorywoda on 03.01.15.
 */
case class NetData(id: String,
                   neurons: List[NeuronData],
                   inputs: List[String],
                   threshold: Double,
                   slope: Double,
                   hushValue: HushValue,
                   forgetting: ForgetTrait,
                   tickTimeMultiplier: Double,
                   weight: SynapseTrait,
                   prefix: String,
                   inputTickMultiplier: Double){
  def toJson = compact(toRawJson)
  def toPrettyJson = pretty(toRawJson) // for debugging purposes only

  def withId(id: String) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, prefix, inputTickMultiplier)
  def withNeurons(neurons: List[NeuronData]) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, prefix, inputTickMultiplier)
  def withInputs(inputs: List[String]) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, prefix, inputTickMultiplier)
  def withThreshold(threshold: Double) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, prefix, inputTickMultiplier)
  def withSlope(slope: Double) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, prefix, inputTickMultiplier)
  def withHushValue(hushValue: HushValue) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, prefix, inputTickMultiplier)
  def withForgetting(forgetting: ForgetTrait) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, prefix, inputTickMultiplier)
  def withTickTimeMultiplier(tickTimeMultiplier: Double) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, prefix, inputTickMultiplier)
  def withWeight(weight: SynapseWeight) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, prefix, inputTickMultiplier)
  def withPrefix(prefix: String) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, prefix, inputTickMultiplier)
  def withInputTickMultiplier(inputTickMultiplier: Double) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, prefix, inputTickMultiplier)

  private def toRawJson = {
    val neuronsJson = neurons.map{ _.toJson }
    val json = ("id" -> id) ~
      ("neurons" -> neuronsJson) ~
      ("inputs" -> inputs) ~
      ("threshold" -> threshold) ~
      ("slope" -> slope) ~
      ("hushValue" -> hushValue.toString) ~
      ("forgetting" -> forgetting.toString) ~
      ("tickTimeMultiplier" -> tickTimeMultiplier) ~
      ("weight" -> weight.toString) ~
      ("prefix" -> prefix) ~
      ("inputTickMultiplier" -> inputTickMultiplier)
    render(json)
  }
}

object NetData {
  def apply(id: String):NetData = NetData(id, Nil, Nil)
  def apply(id: String, neurons: List[NeuronData], inputs: List[String]):NetData =
    NetData(id, neurons, inputs, Context.threshold, Context.slope, Context.hushValue, Context.forgetting, 1.0, Context.weight, "mi", 1.0)

  def fromJson(jsonStr: String):NetData = {
    val json = parse(jsonStr)
    val parsed:List[NetData] = for {
      JObject(data) <- json
      JField("id", JString(id)) <- data
      JField("neurons", JArray(neuronsJson)) <- data
      JField("inputs", JArray(inputsJson)) <- data
      JField("threshold", JDouble(threshold)) <- data
      JField("slope", JDouble(slope)) <- data
      JField("hushValue", JString(hushStr)) <- data
      JField("forgetting", JString(forgettingStr)) <- data
      JField("tickTimeMultiplier", JDouble(tickTimeMultiplier)) <- data
      JField("weight", JString(weightStr)) <- data
      JField("prefix", JString(prefix)) <- data
      JField("inputTickMultiplier", JDouble(inputTickMultiplier)) <- data
    } yield NetData(id, parseNeurons(neuronsJson), parseInputs(inputsJson),
                    threshold, slope, NeuronData.parseHush(hushStr),
                    NeuronData.parseForgetting(forgettingStr), tickTimeMultiplier,
                    SynapseData.parseWeight(weightStr), prefix, inputTickMultiplier)

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
