package anna.epengine

import anna.Context
import anna.async.{NeuronType, NeuronTypeStandard}
import anna.data._
import anna.logger.LOG._
import anna.utils.RandomNumber
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}


/**
 * Created by gorywoda on 28.12.14.
 */
class NeuronGenome(private var _data: NeuronData, val accessMap: Map[String, MutationAccess]) {
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
  def withAccessMap(accessMap: Map[String, MutationAccess]) = NeuronGenome(_data, accessMap)

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
    val newThreshold = RandomNumber(Context().thresholdRange)
    debug(s"MUTATION: mutateThreshold for $id from ${_data.threshold} to $newThreshold")
    _data = _data.withThreshold(newThreshold)
  }

  private def mutateSlope():Unit = {
    val newSlope = RandomNumber(Context().slopeRange)
    debug(s"MUTATION: mutateSlope for $id from ${_data.slope} to $newSlope")
    _data = _data.withSlope(newSlope)
  }

  private def mutateHushValue():Unit = {
    val newHushValue = HushValue(RandomNumber(Context().hushRange))
    debug(s"MUTATION: mutateHushValue for $id from ${_data.hushValue} to $newHushValue")
    _data = _data.withHushValue(newHushValue)
  }

  private def mutateForgetting():Unit = Probability.performRandom(
    (Context().dontForgetProbability, setDontForget _),
    (1.0 - Context().dontForgetProbability - Context().forgetAllProbability, mutateForgetValue _),
    (Context().forgetAllProbability, setForgetAll _)
  )

  private def mutateTickTimeMultiplier():Unit = {
    val newTickTimeMultiplier = RandomNumber(Context().tickTimeMultiplierRange)
    debug(s"MUTATION: mutateTickTimeMultiplier for $id from ${_data.tickTimeMultiplier} to $newTickTimeMultiplier")
    _data = _data.withTickTimeMultiplier(newTickTimeMultiplier)
  }

  private def setDontForget(): Unit ={
    val newForgetting = DontForget()
    debug(s"MUTATION: setDontForget for $id from ${_data.forgetting} to $newForgetting")
    _data = _data.withForgetting(newForgetting)
  }

  private def mutateForgetValue(): Unit ={
    val newForgetting = ForgetValue(RandomNumber(Context().forgettingRange))
    debug(s"MUTATION: mutateForgetValue for $id from ${_data.forgetting} to $newForgetting")
    _data = _data.withForgetting(newForgetting)
  }

  private def setForgetAll(): Unit ={
    val newForgetting = ForgetAll()
    debug(s"MUTATION: setForgetAll for $id from ${_data.forgetting} to $newForgetting")
    _data = _data.withForgetting(newForgetting)
  }

  private def access(neuronId: String) = accessMap.get(neuronId) match {
    case None => MutationAccessFull()
    case Some(value) => value
  }

  private def getRandomNeuronId(onlyMutable: Boolean =true, excludeAlreadyConnected: Boolean =true) = {
    val t = if(onlyMutable){
      accessMap.filter( tuple => tuple._2 != MutationAccessDontMutate() ).map( _._1 ).toList
    } else accessMap.keys.toList

    val neuronIds = if(excludeAlreadyConnected){
      val connectedSet = _data.synapses.map(_.neuronId).toSet
      t.filterNot(connectedSet.contains(_))
    } else t

    if(neuronIds.nonEmpty) Some(RandomNumber(neuronIds)) else None
  }

  private def getRandomSynapse(onlyMutable: Boolean) = {
    val synapses = if(onlyMutable)
      _data.synapses.filter(sd => access(sd.neuronId) != MutationAccessDontMutate())
    else _data.synapses
    if(_data.synapses.nonEmpty) Some(RandomNumber(synapses)) else None
  }

  private def mutateSynapse():Unit = Probability.performRandom(
    (Context().addSynapseProbability, addRandomSynapse _),
    (Context().deleteSynapseProbability, deleteRandomSynapse _),
    (1.0 - Context().addSynapseProbability - Context().deleteSynapseProbability, mutateSynapseWeight _)
  )

  private def addRandomSynapse():Unit = getRandomNeuronId() match {
    case Some(neuronId) =>
      val newSynapse = SynapseGenome.build(neuronId)
      debug(s"MUTATION: addRandomSynapse to $id -> the new synapse connects to ${newSynapse.neuronId} with weight ${newSynapse.weight}")
      addSynapse(newSynapse)
    case _ =>
      debug(this, s"Trying to add a synapse from $id to another neuron, but there are no valid neurons")
  }

  private def deleteRandomSynapse():Unit = getRandomSynapse(true) match {
    case Some(synapse) =>
      debug(s"MUTATION: deleteRandomSynapse from $id -> deleting connection to ${synapse.neuronId}")
      deleteSynapse(synapse.neuronId)
    case None =>
      debug(this, s"Trying to remove a synapse from $id but it's not allowed")
  }

  private def mutateSynapseWeight():Unit = getRandomSynapse(false) match {
      case Some(synapse) =>
        val synapseChromosome = SynapseGenome(synapse)
        debug(s"MUTATION: mutateSynapseWeight for $id ... ")
        synapseChromosome.mutate()
        _data = _data.withSynapses(synapseChromosome.data :: _data.synapses.filterNot(_.neuronId == synapse.neuronId))
      case None =>
        debug(this, s"Trying to mutate a synapse from $id but it's not allowed")
  }

  def connect(to:NeuronGenome): Boolean = if(isConnectedTo(to.id)) false else {
    val sg = SynapseGenome.build(to.id)
    debug(this, s"connecting $id to ${to.id} with ${sg.weight}")
    addSynapse(sg)
    true
  }

  def toJson = writePretty(this)
}

object NeuronGenome {
  def apply(data: NeuronData):NeuronGenome = new NeuronGenome(data, Map())
  def apply(data: NeuronData, accessMap: Map[String, MutationAccess]):NeuronGenome
    = new NeuronGenome(data, accessMap)

  def build(id: String, accessMap: Map[String, MutationAccess] = Map()) = {
    //debug(this,s" --- building neuron $id --- ")

    val ng = NeuronGenome(
      NeuronData(id, Context().thresholdRange.from, Context().slopeRange.from, HushValue(),
                 DontForget(), Nil, Context().tickTimeMultiplierRange.from, NeuronTypeStandard(),
                 Context().activationFunctionName
      ),
      accessMap
    )
    ng.mutateThreshold()
    ng.mutateSlope()
    ng.mutateHushValue()
    ng.mutateForgetting()
    ng.mutateTickTimeMultiplier()

    //debug(this,ng.data.toJson)
    //debug(this,s" --- done building neuron $id --- ")

    ng
  }

  def fromJson(jsonStr: String) = read[NeuronGenome](jsonStr)
}
