package anna.epengine

import anna.async.NeuronType
import anna.data._
import anna.utils.IntRange
import anna.utils.DoubleRange._
import anna.logger.LOG._


/**
 * Created by gorywoda on 28.12.14.
 */
class NeuronGenome(private var _data: NeuronData, val accessMap: Map[String, MutationAccess.Value]) {
  import NeuronGenome._

  def id = _data.id
  def threshold = _data.threshold
  def slope = _data.slope
  def hushValue = _data.hushValue
  def forgetting = _data.forgetting
  def synapses = _data.synapses
  def tickTimeMultiplier = _data.tickTimeMultiplier
  def neuronType = _data.neuronType
  def data = _data

  override def clone = NeuronGenome(_data, accessMap)
  def withAccessMap(accessMap: Map[String, MutationAccess.Value]) = NeuronGenome(_data, accessMap)

  def isConnectedTo(id: String) = _data.isConnectedTo(id)

  def mutate() = Probability.performRandom(
    (thresholdProbability, mutateThreshold _),
    (slopeProbability, mutateSlope _),
    (forgettingProbability, mutateForgetting _),
    (hushProbability, mutateHushValue _),
    (synapseChangeProbability, mutateSynapse _),
    (tickTimeMultiplierProbability, mutateTickTimeMultiplier _)
  )

  def addSynapse(synapseChromosome: SynapseGenome) = {
    _data = _data.withSynapses(synapseChromosome.data :: _data.synapses)
  }

  def deleteSynapse(neuronId: String) = {
    debug(this, s"deleteSynapse, neuronId: $neuronId")
    _data = _data.withSynapses(_data.synapses.filterNot(_.neuronId == neuronId))
  }
  
  def getSynapse(neuronId: String) = _data.synapses.find( _.neuronId == neuronId ) match {
    case Some(synapse) => synapse
    case None => throw new IllegalArgumentException(s"There is no synapse connecting ${_data.id} with $neuronId")
  }

  private def mutateThreshold():Unit = {
    _data = _data.withThreshold(thresholdRange.choose(RandomNumber()))
  }

  private def mutateSlope():Unit = {
    _data = _data.withSlope(slopeRange.choose(RandomNumber()))
  }

  private def mutateHushValue():Unit = {
    _data = _data.withHushValue(HushValue(hushRange.choose(RandomNumber())))
  }

  private def mutateForgetting():Unit = Probability.performRandom(
    (dontForgetProbability, setDontForget _),
    (1.0 - dontForgetProbability - forgetAllProbability, mutateForgetValue _),
    (forgetAllProbability, setForgetAll _)
  )


  private def mutateTickTimeMultiplier():Unit = {
    _data = _data.withTickTimeMultiplier(tickTimeMultiplierRange.choose(RandomNumber()))
  }

  private def setDontForget(): Unit ={
    _data = _data.withForgetting(DontForget)
  }

  private def mutateForgetValue(): Unit ={
    _data = _data.withForgetting(ForgetValue(forgettingRange.choose(RandomNumber())))
  }

  private def setForgetAll(): Unit ={
    _data = _data.withForgetting(ForgetAll)
  }

  private def access(neuronId: String) = accessMap.get(neuronId) match {
    case None => MutationAccess.FULL
    case Some(value) => value
  }

  private def getRandomNeuronId(fullAccess: Boolean =true, excludeAlreadyConnected: Boolean =true) = {
    val t = if(fullAccess){
      accessMap.filter( tuple => tuple._2 == MutationAccess.FULL ).map( _._1 ).toList
    } else accessMap.keys.toList

    val neuronIds = if(excludeAlreadyConnected){
      val connectedSet = _data.synapses.map(_.neuronId).toSet
      t.filterNot(connectedSet.contains(_))
    } else t

    if(neuronIds.nonEmpty) {
      val randomIndex = (0 until neuronIds.size).choose(RandomNumber())
      Some(neuronIds(randomIndex))
    } else None
  }

  private def getRandomSynapse(fullAccess: Boolean) = {
    debug(this,s"getRandomSynapses, fullAccess:$fullAccess")
    val synapses = if(fullAccess)
      _data.synapses.filter(sd => access(sd.neuronId) == MutationAccess.FULL)
    else _data.synapses
    if(_data.synapses.nonEmpty) {
      Some(synapses(RandomNumber(synapses.size)))
    } else None
  }

  private def mutateSynapse():Unit = Probability.performRandom(
    (addSynapseProbability, addRandomSynapse _),
    (deleteSynapseProbability, deleteRandomSynapse _),
    (1.0 - addSynapseProbability - deleteSynapseProbability, mutateSynapseWeight _)
  )

  private def addRandomSynapse():Unit = getRandomNeuronId() match {
    case Some(neuronId) => addSynapse(Engine().tossForSynapse(neuronId))
    case _ => debug(this, s"Trying to add a synapse from $id to another neuron, but there are no valid neurons")
  }

  private def deleteRandomSynapse():Unit = getRandomSynapse(true) match {
    case Some(synapse) => deleteSynapse(synapse.neuronId)
    case None => debug(this, s"Trying to remove a synapse from $id but it's not allowed")
  }

  private def mutateSynapseWeight():Unit = getRandomSynapse(false) match {
      case Some(synapse) => val synapseChromosome = SynapseGenome(synapse)
                            synapseChromosome.mutate()
                            _data = _data.withSynapses(synapseChromosome.data :: _data.synapses.filterNot(_.neuronId == synapse.neuronId))
      case None => debug(this, s"Trying to mutate a synapse from $id but it's not allowed")
  }
}

object NeuronGenome {
  var thresholdRange = 0.0<=>0.9
  var slopeRange = 5.0<=>20.0
  var hushRange = 1 to 5
  var forgettingRange = 0.1<=>0.9
  var dontForgetProbability = Probability(0.75)
  var forgetAllProbability = Probability(0.05)
  var tickTimeMultiplierRange = 1.0<=>1.0

  var thresholdProbability = Probability(0.1)
  var slopeProbability = Probability(0.1)
  var forgettingProbability = Probability(0.1)
  var hushProbability = Probability(0.05)
  var synapseChangeProbability = Probability(0.6)
  var tickTimeMultiplierProbability = Probability(0.05)

  var addSynapseProbability = Probability(0.1)
  var deleteSynapseProbability = Probability(0.1)

  def apply(data: NeuronData):NeuronGenome = new NeuronGenome(data, Map())
  def apply(data: NeuronData, accessMap: Map[String, MutationAccess.Value]):NeuronGenome
    = new NeuronGenome(data, accessMap)
  def apply(id: String,
            threshold: Double,
            slope: Double,
            hushValue: HushValue,
            forgetting: ForgetTrait,
            synapses: List[SynapseData],
            tickTimeMultiplier: Double,
            neuronType: NeuronType.Value):NeuronGenome =
    NeuronGenome(NeuronData(id, threshold, slope, hushValue, forgetting, synapses, tickTimeMultiplier, neuronType))

  def toss(id: String, accessMap: Map[String, MutationAccess.Value] = Map()) = {
    val nch = NeuronGenome(
      NeuronData(id, thresholdRange.from, slopeRange.from, HushValue(), DontForget, Nil, tickTimeMultiplierRange.from, NeuronType.STANDARD),
      accessMap
    )
    nch.mutateThreshold()
    nch.mutateSlope()
    nch.mutateHushValue()
    nch.mutateForgetting()
    nch.mutateTickTimeMultiplier()
    nch
  }

  implicit private def fromRange(r: Range):IntRange = IntRange(r)
}
