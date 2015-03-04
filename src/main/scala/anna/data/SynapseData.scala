package anna.data

import anna.logger.LOG._
import anna.utils.DoubleRange._
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._

sealed trait SynapseTrait extends Any
case class SynapseWeight(weight: Double) extends AnyVal with SynapseTrait {
  def check = assert((-1.0<=>1.0).contains(weight))
}
case object Hush extends SynapseTrait

case class SynapseData(neuronId: String, weight: SynapseTrait){
  def toJson = compact(toRawJson)
  def toPrettyJson = pretty(toRawJson) // for debug purposes only

  def withId(neuronId: String) = SynapseData(neuronId, weight)
  def withWeight(weight: SynapseTrait) = SynapseData(neuronId, weight)

  private def toRawJson = render(("neuronId" -> neuronId) ~ ("weight" -> weight.toString))
}

object SynapseData {
  def apply(neuronId: String, weight: Double):SynapseData = SynapseData(neuronId, SynapseWeight(weight))

  def fromJson(jsonStr: String) = {
    val json = parse(jsonStr)

    val parsed:List[SynapseData] = for {
      JObject(data) <- json
      JField("neuronId", JString(neuronId)) <- data
      JField("weight", JString(weightStr)) <- data
    } yield apply(neuronId, parseWeight(weightStr))

    if(parsed.size != 1) exception(this, s"Unable to parse JSON: $jsonStr")
    parsed(0)
  }

  private val weightr = """SynapseWeight\(([0-9\.\-]+)\)""".r
  private val hushr = Hush.toString

  def parseWeight(weightStr: String):SynapseTrait = weightStr match {
    case `hushr` => Hush
    case weightr(w) => SynapseWeight(w.toDouble)
  }

  implicit def fromDouble(weight: Double):SynapseWeight = SynapseWeight(weight)
}