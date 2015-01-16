package anna.epengine

import anna.async.NeuronType
import anna.data._
import anna.utils.IntRange
import anna.utils.DoubleRange._
import anna.logger.LOG._


/**
 * Created by gorywoda on 28.12.14.
 */
class NeuronChromosome(private var data: NeuronData, val neuronAccessMap: Map[String, MutationAccess.Value]) {
  def id = data.id
  def threshold = data.threshold
  def slope = data.slope
  def hushValue = data.hushValue
  def forgetting = data.forgetting
  def synapses = data.synapses
  def tickTimeMultiplier = data.tickTimeMultiplier
  def neuronType = data.neuronType
  def neuron = data

  var thresholdRange = 0.0<=>0.9
  var slopeRange = 5.0<=>20.0
  var hushRange = 1 to 5
  var forgettingRange = 0.1<=>0.9
  var dontForgetProbability = 0.75
  var forgetAllProbability = 0.05
  var tickTimeMultiplierRange = 0.5<=>2.0

  var thresholdProbability = Probability(0.1)
  var slopeProbability = Probability(0.1)
  var forgettingProbability = Probability(0.1)
  var hushProbability = Probability(0.05)
  var synapseChangeProbability = Probability(0.65)

  var addSynapseProbability = Probability(0.1)
  var removeSynapseProbability = Probability(0.1)

  def isConnectedTo(id: String) = synapses.find(_.neuronId == id) != None

  def mutate():NeuronChromosome = {
    data = Probability.chooseOne(thresholdProbability, slopeProbability, forgettingProbability, hushProbability, synapseChangeProbability) match {
      case 0 => data.withThreshold(thresholdRange.choose(RandomNumber()))
      case 1 => data.withSlope(slopeRange.choose(RandomNumber()))
      case 2 => mutateForgetting()
      case 3 => data.withHushValue(HushValue(hushRange.choose(RandomNumber())))
      case 4 => mutateSynapse()
    }
    this
  }

  def addSynapse(synapseChromosome: SynapseChromosome) = {
    data = data.withSynapses(synapseChromosome.synapse :: data.synapses)
  }

  private def mutateForgetting() = {
    val forgetValueProbability = 1.0 - dontForgetProbability - forgetAllProbability
    data.withForgetting(Probability.chooseOne(dontForgetProbability, forgetValueProbability, forgetAllProbability) match {
      case 0 => DontForget
      case 1 => ForgetValue(forgettingRange.choose(RandomNumber()))
      case 2 => ForgetAll
    })
  }

  private def access(neuronId: String) = neuronAccessMap.get(neuronId) match {
    case None => MutationAccess.FULL
    case Some(value) => value
  }

  private def getRandomNeuronId(fullAccess: Boolean) = {
    val neuronIds = if(fullAccess){
      neuronAccessMap.filter( tuple => tuple._2 == MutationAccess.FULL ).map( _._1 ).toList
    } else neuronAccessMap.keys.toList
    if(neuronIds.nonEmpty) {
      val randomIndex = (0 until neuronIds.size).choose(RandomNumber())
      Some(neuronIds(randomIndex))
    } else None
  }

  private def getRandomSynapse(fullAccess: Boolean) = {
    val synapses = if(fullAccess)
      data.synapses.filter(sd => access(sd.neuronId) == MutationAccess.FULL)
    else data.synapses
    val randomIndex = (0 until synapses.size).choose(RandomNumber())
    synapses(randomIndex)
  }

  private def mutateSynapse() = {
    val changeWeightProbability = 1.0 - addSynapseProbability - removeSynapseProbability
    Probability.chooseOne(addSynapseProbability, removeSynapseProbability, changeWeightProbability) match {
      case 0 => getRandomNeuronId(true) match {
        case Some(neuronId) => val synapseChromosome = Engine().tossForSynapse(neuronId)
                               data.withSynapses(synapseChromosome.synapse :: data.synapses)
        case _ => exception(this, s"Trying to add a synapse from $id to another neuron, but there are no valid neurons")
                  data // unreachable code
      }
      case 1 if data.synapses.nonEmpty =>
        val neuronId = getRandomSynapse(true).neuronId
        data.withSynapses(data.synapses.filterNot(_.neuronId == neuronId))
      case 2 if data.synapses.nonEmpty =>
        val synapse = getRandomSynapse(false)
        val synapseChromosome = SynapseChromosome(synapse).mutate()
        data.withSynapses(synapseChromosome.synapse :: data.synapses.filterNot(_.neuronId == synapse.neuronId))
      case _ => data
    }
  }

  implicit private def fromRange(r: Range):IntRange = IntRange(r)
}

object NeuronChromosome {
  def apply(data: NeuronData):NeuronChromosome = new NeuronChromosome(data, Map())
  def apply(data: NeuronData, accessMap: Map[String, MutationAccess.Value]):NeuronChromosome = new NeuronChromosome(data, accessMap)
  def apply(id: String,
            threshold: Double,
            slope: Double,
            hushValue: HushValue,
            forgetting: ForgetTrait,
            synapses: List[SynapseData],
            tickTimeMultiplier: Double,
            neuronType: NeuronType.Value):NeuronChromosome =
    NeuronChromosome(NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, neuronType))
}
