package anna.epengine

import anna.Context
import anna.async.NeuronType
import anna.data._
import anna.logger.LOG._
import anna.utils.RandomNumber

import anna.logger.LOG._

/**
 * Created by gorywoda on 28.12.14.
 */
class NeuronGenome(private var _data: NeuronData, val accessMap: Map[String, MutationAccess.Value]) {
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
    (Context().thresholdProbability, mutateThreshold _),
    (Context().slopeProbability, mutateSlope _),
    (Context().forgettingProbability, mutateForgetting _),
    (Context().hushValueProbability, mutateHushValue _),
    (Context().synapseChangeProbability, mutateSynapse _),
    (Context().tickTimeMultiplierProbability, mutateTickTimeMultiplier _)
  )

  def addSynapse(synapseChromosome: SynapseGenome) = {
    _data = _data.withSynapses(synapseChromosome.data :: _data.synapses)
  }

  def deleteSynapse(neuronId: String) = {
    _data = _data.withSynapses(_data.synapses.filterNot(_.neuronId == neuronId))
  }
  
  def getSynapse(neuronId: String) = _data.synapses.find( _.neuronId == neuronId ) match {
    case Some(synapse) => synapse
    case None => throw new IllegalArgumentException(s"There is no synapse connecting ${_data.id} with $neuronId")
  }

  private def mutateThreshold():Unit = {
    _data = _data.withThreshold(RandomNumber(Context().thresholdRange))
  }

  private def mutateSlope():Unit = {
    _data = _data.withSlope(RandomNumber(Context().slopeRange))
  }

  private def mutateHushValue():Unit = {
    _data = _data.withHushValue(HushValue(RandomNumber(Context().hushRange)))
  }

  private def mutateForgetting():Unit = Probability.performRandom(
    (Context().dontForgetProbability, setDontForget _),
    (1.0 - Context().dontForgetProbability - Context().forgetAllProbability, mutateForgetValue _),
    (Context().forgetAllProbability, setForgetAll _)
  )

  private def mutateTickTimeMultiplier():Unit = {
    _data = _data.withTickTimeMultiplier(RandomNumber(Context().tickTimeMultiplierRange))
  }

  private def setDontForget(): Unit ={
    _data = _data.withForgetting(DontForget)
  }

  private def mutateForgetValue(): Unit ={
    _data = _data.withForgetting(ForgetValue(RandomNumber(Context().forgettingRange)))
  }

  private def setForgetAll(): Unit ={
    _data = _data.withForgetting(ForgetAll)
  }

  private def access(neuronId: String) = accessMap.get(neuronId) match {
    case None => MutationAccess.FULL
    case Some(value) => value
  }

  private def getRandomNeuronId(onlyMutable: Boolean =true, excludeAlreadyConnected: Boolean =true) = {
    val t = if(onlyMutable){
      accessMap.filter( tuple => tuple._2 != MutationAccess.DONTMUTATE ).map( _._1 ).toList
    } else accessMap.keys.toList

    val neuronIds = if(excludeAlreadyConnected){
      val connectedSet = _data.synapses.map(_.neuronId).toSet
      t.filterNot(connectedSet.contains(_))
    } else t

    if(neuronIds.nonEmpty) Some(RandomNumber(neuronIds)) else None
  }

  private def getRandomSynapse(onlyMutable: Boolean) = {
    val synapses = if(onlyMutable)
      _data.synapses.filter(sd => access(sd.neuronId) != MutationAccess.DONTMUTATE)
    else _data.synapses
    if(_data.synapses.nonEmpty) Some(RandomNumber(synapses)) else None
  }

  private def mutateSynapse():Unit = Probability.performRandom(
    (Context().addSynapseProbability, addRandomSynapse _),
    (Context().deleteSynapseProbability, deleteRandomSynapse _),
    (1.0 - Context().addSynapseProbability - Context().deleteSynapseProbability, mutateSynapseWeight _)
  )

  private def addRandomSynapse():Unit = getRandomNeuronId() match {
    case Some(neuronId) => addSynapse(SynapseGenome.build(neuronId))
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

  def connect(to:NeuronGenome): Boolean = if(isConnectedTo(to.id)) false else {
    val sg = SynapseGenome.build(to.id)
    debug(this, s"connecting $id to ${to.id} with ${sg.weight}")
    addSynapse(sg)
    true
  }
}

object NeuronGenome {
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

  def build(id: String, accessMap: Map[String, MutationAccess.Value] = Map()) = {
    debug(this,s" --- building neuron $id --- ")

    val ng = NeuronGenome(
      NeuronData(id, Context().thresholdRange.from, Context().slopeRange.from, HushValue(),
                 DontForget, Nil, Context().tickTimeMultiplierRange.from, NeuronType.STANDARD),
      accessMap
    )
    ng.mutateThreshold()
    ng.mutateSlope()
    ng.mutateHushValue()
    ng.mutateForgetting()
    ng.mutateTickTimeMultiplier()

    debug(this,ng.data.toJson)
    debug(this,s" --- done building neuron $id --- ")

    ng
  }
}
