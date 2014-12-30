package anna.data

import anna.async.NeuronType
import anna.async.logger.LOG._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

case class HushValue(iterations: Int = 1) extends AnyVal
  
sealed trait ForgetTrait extends Any
case class ForgetValue(value: Double) extends AnyVal with ForgetTrait 
case object ForgetAll extends ForgetTrait
case object DontForget extends ForgetTrait

case class NeuronData(
    id: String,
    threshold: Double,
    slope: Double,
    hushValue: HushValue,
    forgetting: ForgetTrait,
    synapses: List[SynapseData],
    tickTime: Long,
    neuronType: NeuronType.Value
){
  def toJson = compact(toRawJson)
  def toPrettyJson = pretty(toRawJson) // for debugging purposes only

  def withId(id: String) = NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTime, neuronType)
  def withThreshold(threshold: Double) = NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTime, neuronType)
  def withSlope(slope: Double) = NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTime, neuronType)
  def withHushValue(hushValue: HushValue) = NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTime, neuronType)
  def withForgetting(forgetting: ForgetTrait) = NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTime, neuronType)
  def withSynapses(synapses: List[SynapseData]) = NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTime, neuronType)
  def withNeuronType(neuronType: NeuronType.Value) = NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTime, neuronType)

  private def toRawJson = {
    val synapsesJson = synapses.map{ _.toJson }
    val json = ("id" -> id) ~
               ("threshold" -> threshold) ~
               ("slope" -> slope) ~
               ("hushValue" -> hushValue.toString) ~
               ("forgetting" -> forgetting.toString) ~
               ("synapses" -> synapsesJson) ~
               ("tickTime" -> tickTime) ~
               ("neuronType" -> neuronType.toString)
    render(json)
  }
} 

object NeuronData {
  def apply(id: String, 
            threshold: Double, 
            slope: Double, 
            hushValue: HushValue, 
            forgetting: ForgetTrait,
            synapses: List[SynapseData],
            tickTime: Long):NeuronData
    = apply(id, threshold, slope, hushValue, forgetting, synapses, tickTime, NeuronType.STANDARD)

  def apply(id: String, 
            threshold: Double, 
            slope: Double, 
            hushValue: HushValue,
            forgetting: ForgetTrait,
            tickTime: Long):NeuronData
    = apply(id, threshold, slope, hushValue, forgetting, Nil, tickTime, NeuronType.STANDARD)

  def apply(id: String,  
            hushValue: HushValue,
            tickTime: Long):NeuronData
    = apply(id, 0.0, 0.0, hushValue, ForgetAll, Nil, tickTime, NeuronType.DUMMY)
  
  def apply(id: String):NeuronData
    = apply(id, 0.0, 0.0, HushValue(), ForgetAll, Nil, 0L, NeuronType.HUSH)

  def fromJson(jsonStr: String):NeuronData = {
    val json = parse(jsonStr)

    val parsed:List[NeuronData] = for {
      JObject(data) <- json
      JField("id", JString(id)) <- data
      JField("threshold", JDouble(threshold)) <- data
      JField("slope", JDouble(slope)) <- data
      JField("hushValue", JString(hushStr)) <- data
      JField("forgetting", JString(forgetStr)) <- data
      JField("synapses", JArray(synapsesJson)) <- data
      JField("tickTime", JInt(tickTime)) <- data
      JField("neuronType", JString(neuronTypeStr)) <- data
    } yield NeuronData(id, threshold, slope, parseHush(hushStr), parseForgetting(forgetStr),
                       parseSynapses(synapsesJson), tickTime.toLong, NeuronType.parse(neuronTypeStr))

    if(parsed.size != 1) exception(this, s"Unable to parse JSON: $jsonStr")
    parsed(0)
  }

  private val hushr = """HushValue\(([0-9]+)\)""".r

  private def parseHush(hushStr: String) = hushStr match {
    case hushr(h) => HushValue(h.toInt)
  }

  private val forgetr = """ForgetValue\(([0-9\.\-]+)\)""".r
  private val dontforgetr = DontForget.toString
  private val forgetallr = ForgetAll.toString

  private def parseForgetting(forgetStr: String) = forgetStr match {
    case `dontforgetr` => DontForget
    case `forgetallr` => ForgetAll
    case forgetr(f) => ForgetValue(f.toDouble)
  }

  private def parseSynapses(synapsesJson: List[JValue]) = synapsesJson.map {
    case JString(s) => SynapseData.fromJson(s)
    case _ => throw new IllegalArgumentException(s"Unable to parse JSON $synapsesJson")
  }
}