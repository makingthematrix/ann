package anna.data
import org.json4s.JsonDSL._
import anna.logger.LOG._
import org.json4s._
import org.json4s.native.JsonMethods._

case class SynapseData(neuronId: String, weight: SynapseTrait){
  def withId(neuronId: String) = SynapseData(neuronId, weight)
  def withWeight(weight: SynapseTrait) = SynapseData(neuronId, weight)

  def toJson = pretty(render(("neuronId" -> neuronId) ~ ("weight" -> weight.toString)))
}

object SynapseData {
  def apply(neuronId: String, weight: Double):SynapseData = SynapseData(neuronId, SynapseWeight(weight))

  def fromJson(jsonStr: String) = {
    val json = parse(jsonStr)

    val parsed:List[SynapseData] = for {
      JObject(data) <- json
      JField("neuronId", JString(neuronId)) <- data
      JField("weight", JString(weightStr)) <- data
    } yield apply(neuronId, SynapseTrait(weightStr))

    if(parsed.size != 1) exception(this, s"Unable to parse JSON: $jsonStr")
    parsed(0)
  }

  implicit def fromDouble(weight: Double):SynapseWeight = SynapseWeight(weight)
}