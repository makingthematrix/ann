package anna.epengine

import anna.Context
import anna.async.{NeuronType, NeuronTypeStandard}
import anna.data._
import anna.logger.LOG._
import anna.utils.RandomNumber
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}
import scala.collection.mutable

/**
 * Created by gorywoda on 28.12.14.
 */
class NeuronGenome(var id: String,
                   var threshold: Double,
                   var slope: Double,
                   var hushValue: HushValue,
                   var forgetting: ForgetTrait,
                   var synapses: mutable.ListBuffer[SynapseGenome],
                   var tickTimeMultiplier: Double,
                   val neuronType: NeuronType,
                   val activationFunctionName: String) {
  def isConnectedTo(id: String) = synapses.exists(s => s.neuronId == id || NetData.removeNetId(s.neuronId) == id)

  def addSynapse(gen: SynapseGenome) = {
    debug(this, s"adding a synapse from $id to ${gen.neuronId}")
    synapses += gen
  }

  def deleteSynapse(neuronId: String) = synapses.find(_.neuronId == neuronId) match {
    case Some(s) => synapses -= s
    case None =>
  }
  
  def getSynapse(neuronId: String):SynapseGenome = synapses.find(s => s.neuronId == neuronId || NetData.removeNetId(s.neuronId) == neuronId) match {
    case Some(synapse) => synapse
    case None => throw new IllegalArgumentException(s"There is no synapse connecting $id with $neuronId")
  }

  def connect(to:NeuronGenome) = if(isConnectedTo(to.id)) None else {
    val sg = SynapseGenome.build(to.id)
    debug(this, s"connecting $id to ${to.id} with ${sg.weight}")
    addSynapse(sg)
    Some(sg)
  }

  def toJson = writePretty(this)
  def data = NeuronData(id, threshold, slope, hushValue, forgetting,
                        synapses.map(_.data).toList,
                        tickTimeMultiplier, neuronType, activationFunctionName)
  override def clone = new NeuronGenome(id, threshold, slope, hushValue, forgetting, synapses.map(_.clone),
                                        tickTimeMultiplier, neuronType, activationFunctionName)
}

object NeuronGenome {
  def apply(id: String, threshold: Double, slope: Double, hushValue: HushValue, forgetting: ForgetTrait,
            synapses: List[SynapseData], tickTimeMultiplier: Double, neuronType: NeuronType, activationFunctionName: String) = {
    val sListBuffer = mutable.ListBuffer[SynapseGenome]()
    sListBuffer ++= synapses.map(s => SynapseGenome(s))
    new NeuronGenome(id, threshold, slope, hushValue, forgetting, sListBuffer, tickTimeMultiplier, neuronType, activationFunctionName)
  }
  def apply(gen: NeuronGenome):NeuronGenome = {
    val sListBuffer = mutable.ListBuffer[SynapseGenome]()
    sListBuffer ++= gen.synapses.map(s => SynapseGenome(s))
    new NeuronGenome(gen.id, gen.threshold, gen.slope, gen.hushValue, gen.forgetting, sListBuffer, gen.tickTimeMultiplier, gen.neuronType, gen.activationFunctionName)
  }

  def apply(data: NeuronData):NeuronGenome = apply(
    data.id, data.threshold, data.slope, data.hushValue, data.forgetting, data.synapses, data.tickTimeMultiplier, data.neuronType, data.activationFunctionName
  )

  def build(id: String) = {
    val threshold = RandomNumber(Context().thresholdRange)
    val slope = RandomNumber(Context().slopeRange)
    val hushValue = HushValue(RandomNumber(Context().hushRange))
    var forgetting: ForgetTrait = DontForget()
    val t = ForgetValue(RandomNumber(Context().forgettingRange))
    Probability.performRandom(
      (Context().dontForgetProbability, () => {}),
      (Context().forgetAllProbability, () => { forgetting = ForgetAll() }),
      (1.0 - Context().dontForgetProbability - Context().forgetAllProbability, () => { forgetting = ForgetValue(RandomNumber(Context().forgettingRange))})
    )
    val tickTimeMultiplier = RandomNumber(Context().tickTimeMultiplierRange)
    NeuronGenome(id, threshold, slope, hushValue, forgetting, Nil, tickTimeMultiplier, NeuronTypeStandard(), Context().activationFunctionName)
  }

  def fromJson(jsonStr: String) = read[NeuronGenome](jsonStr)
}
