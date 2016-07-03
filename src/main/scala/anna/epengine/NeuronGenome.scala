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
                   var hushValue: HushValue,
                   var forgetting: ForgetTrait,
                   var synapses: mutable.ListBuffer[SynapseGenome],
                   val neuronType: NeuronType,
                   val activationFunctionName: String,
                   var friends: mutable.Set[String]
                  ) {
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
  def data = NeuronData(id, threshold, hushValue, forgetting,
                        synapses.map(_.data).toList, neuronType, activationFunctionName, friends.toSet)
  override def clone = new NeuronGenome(id, threshold, hushValue, forgetting, synapses.map(_.clone),
                                        neuronType, activationFunctionName, friends)
}

object NeuronGenome {
  def apply(id: String, threshold: Double, hushValue: HushValue, forgetting: ForgetTrait,
            synapses: List[SynapseData], neuronType: NeuronType, activationFunctionName: String, friends: Set[String]) = {
    val sListBuffer = mutable.ListBuffer[SynapseGenome]()
    sListBuffer ++= synapses.map(s => SynapseGenome(s))
    val friendSet = mutable.Set[String]()
    friendSet ++= friends
    new NeuronGenome(id, threshold, hushValue, forgetting, sListBuffer, neuronType, activationFunctionName, friendSet)
  }

  def apply(gen: NeuronGenome):NeuronGenome = {
    val sListBuffer = mutable.ListBuffer[SynapseGenome]()
    sListBuffer ++= gen.synapses.map(s => SynapseGenome(s))
    val friendSet = mutable.Set[String]()
    friendSet ++= gen.friends
    new NeuronGenome(gen.id, gen.threshold, gen.hushValue, gen.forgetting, sListBuffer, gen.neuronType, gen.activationFunctionName, friendSet)
  }

  def apply(data: NeuronData):NeuronGenome = apply(
    data.id, data.threshold, data.hushValue, data.forgetting, data.synapses, data.neuronType, data.activationFunctionName, data.friends
  )

  def build(id: String) = {
    val threshold = RandomNumber(Context().thresholdRange)
    val hushValue = HushValue(RandomNumber(Context().hushRange))
    var forgetting: ForgetTrait = DontForget()
    val t = ForgetValue(RandomNumber(Context().forgettingRange))
    Probability.performRandom(
      (Context().dontForgetProbability, () => {}),
      (Context().forgetAllProbability, () => { forgetting = ForgetAll() }),
      (1.0 - Context().dontForgetProbability - Context().forgetAllProbability, () => { forgetting = ForgetValue(RandomNumber(Context().forgettingRange))})
    )
    NeuronGenome(id, threshold, hushValue, forgetting, Nil, NeuronTypeStandard(), Context().activationFunctionName, Set[String]())
  }

  def fromJson(jsonStr: String) = read[NeuronGenome](jsonStr)
}
