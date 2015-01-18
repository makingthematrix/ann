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

  def mutate() = Probability.performRandom(
    (thresholdProbability, mutateThreshold _),
    (slopeProbability, mutateSlope _),
    (forgettingProbability, mutateForgetting _),
    (hushProbability, mutateHushValue _),
    (synapseChangeProbability, mutateSynapse _)
  )

  def addSynapse(synapseChromosome: SynapseChromosome) = {
    data = data.withSynapses(synapseChromosome.synapse :: data.synapses)
  }

  private def mutateThreshold():Unit = {
    data = data.withThreshold(thresholdRange.choose(RandomNumber()))
  }

  private def mutateSlope():Unit = {
    data = data.withSlope(slopeRange.choose(RandomNumber()))
  }

  private def mutateHushValue():Unit = {
    data = data.withHushValue(HushValue(hushRange.choose(RandomNumber())))
  }

  private def mutateForgetting():Unit = Probability.performRandom(
    (dontForgetProbability, setDontForget _),
    (1.0 - dontForgetProbability - forgetAllProbability, mutateForgetValue _),
    (forgetAllProbability, setForgetAll _)
  )

  private def setDontForget(): Unit ={
    data = data.withForgetting(DontForget)
  }

  private def mutateForgetValue(): Unit ={
    data = data.withForgetting(ForgetValue(forgettingRange.choose(RandomNumber())))
  }

  private def setForgetAll(): Unit ={
    data = data.withForgetting(ForgetAll)
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
    if(data.synapses.nonEmpty) {
      val randomIndex = (0 until synapses.size).choose(RandomNumber())
      Some(synapses(randomIndex))
    } else None
  }

  private def mutateSynapse():Unit = Probability.performRandom(
    (addSynapseProbability, addRandomSynapse _),
    (removeSynapseProbability, removeRandomSynapse _),
    (1.0 - addSynapseProbability - removeSynapseProbability, mutateSynapseWeight _)
  )

  private def addRandomSynapse():Unit = getRandomNeuronId(true) match {
    case Some(neuronId) => val synapseChromosome = Engine().tossForSynapse(neuronId)
                           data = data.withSynapses(synapseChromosome.synapse :: data.synapses)
    case _ => debug(this, s"Trying to add a synapse from $id to another neuron, but there are no valid neurons")
  }

  private def removeRandomSynapse():Unit = getRandomSynapse(true) match {
    case Some(synapse) => data = data.withSynapses(data.synapses.filterNot(_.neuronId == synapse.neuronId))
    case None => debug(this, s"Trying to remove a synapse from $id but it's not allowed")
  }

  private def mutateSynapseWeight():Unit = getRandomSynapse(false) match {
      case Some(synapse) => val synapseChromosome = SynapseChromosome(synapse)
                            synapseChromosome.mutate()
                            data = data.withSynapses(synapseChromosome.synapse :: data.synapses.filterNot(_.neuronId == synapse.neuronId))
      case None => debug(this, s"Trying to mutate a synapse from $id but it's not allowed")
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
