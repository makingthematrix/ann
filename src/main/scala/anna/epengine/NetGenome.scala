package anna.epengine

import anna.Context
import anna.data.NetData._
import anna.data.{NetData, NeuronData}
import anna.logger.LOG._
import anna.utils.Utils.formats
import anna.utils.{IntRange, RandomNumber, Utils}
import org.json4s.native.Serialization.{read, writePretty}

import scala.annotation.tailrec

/**
 * Created by gorywoda on 04.01.15.
 */

class NetGenome(private var _data: NetData, val accessMap: Map[String, MutationAccess]){
  override def clone = NetGenome(_data, accessMap)

  def id = _data.id
  def neurons = _data.neurons
  def inputs = _data.inputs
  def inputTickMultiplier = _data.inputTickMultiplier
  def data = _data
  def find(id: String) = _data.neurons.find( _.id == id )
  def filter(ids: Seq[String]) = _data.filter(ids)
  def filterNot(ids: Seq[String]) = _data.filterNot(ids)

  def findSynapse(from: String, to: String) = find(from) match {
    case Some(n) => n.synapses.find( _.neuronId == to )
    case None => None
  }

  def fullAccessNeurons() = neurons.filter(n => accessMap.getOrElse(n.id, MutationAccessFull()) == MutationAccessFull())
  def mutableNeurons() = neurons.filter( n => accessMap.getOrElse(n.id, MutationAccessFull()) != MutationAccessDontMutate())
  def notFullAccessNeurons() = neurons.filterNot(n => accessMap.getOrElse(n.id, MutationAccessFull()) == MutationAccessFull())

  def netId(newNetId: String) = {
    _data = _data.withNewNetId(newNetId)
    this
  }

  def mutate(repetitions: Int = 1) = {
    for(i <- 0 to repetitions) Probability.performRandom(
      (Context().addNeuronProbability, addNeuron _),
      (Context().deleteNeuronProbability, deleteNeuron _),
      (Context().mutateNeuronProbability, mutateNeuron _),
      (Context().inputTickMultiplierProbability, mutateInputTickMultiplier _)
    )
    this
  }

  def trim():Unit = {
    // identify synapses which lead to non-existing neurons
    // for each such synapse, create a neuron without it and replace the original one with it in data
    val neuronIdSet = neurons.map(_.id).toSet

    var replaceNeeded = false
    val trimmedNeurons = neurons.map(n => {
      val trimmedSynapses = n.synapses.filter( s => neuronIdSet.contains(s.neuronId))
      if(trimmedSynapses.size == n.synapses.size) n else {
        replaceNeeded = true
        n.withSynapses(trimmedSynapses)
      }
    })
    if(replaceNeeded) _data = _data.withNeurons(trimmedNeurons)

    // identify full access neurons which have no synapses leading to them
    // delete these neurons from the data
    val endPointNeuronIds = neurons.map( _.synapses.map(_.neuronId) ).flatten.toSet
    val endPointNeurons = neurons.filter( n =>
      endPointNeuronIds.contains(n.id) ||
       accessMap.getOrElse(n.id, MutationAccessFull()) != MutationAccessFull()
    )
    if(endPointNeurons.size != neurons.size) _data = _data.withNeurons(endPointNeurons)
  }

  def crossable(genome: NetGenome) = {
    val constants1 = notFullAccessNeurons()
    val constants2 = genome.notFullAccessNeurons()

    assert(
      constants1.map(_.id).toSet == constants2.map(_.id).toSet,
      s"Unable to cross $id with ${genome.id}: The list of constant neurons differ, they are respectively ${constants1.map(_.id).sorted} and ${constants2.map(_.id).sorted}"
    )

    val var1Ids = fullAccessNeurons().map(n => removeNetId(n.id)).toSet
    val var2Ids = genome.fullAccessNeurons().map(n => removeNetId(n.id)).toSet
    // if there is no common part then we cannot cross the genomes
    var1Ids.intersect(var2Ids).nonEmpty
  }

  def cross(genome: NetGenome, trimEnabled: Boolean =true, renameEnabled: Boolean =true) = if(crossable(genome)) {
    debug(this, s"--- crossing ${this.id} with ${genome.id}")
    val var1 = fullAccessNeurons()
    val var2 = genome.fullAccessNeurons()

    // 1. a variable neuron id should be as follows: [netId]_[neuronId]. strip netId and assume that neurons from both
    // nets with the same neuronId are equivalent and so, after the cross they cannot end up in the same new net.
    val var1Ids = var1.map(n => removeNetId(n.id)).toSet
    val var2Ids = var2.map(n => removeNetId(n.id)).toSet
    val commonIds = var1Ids.intersect(var2Ids)

    // if crossable is true it means that there is a common part which we can cross

    // 2. choose the number of neuronIds to draw
    val idsToDraw = if (commonIds.size > 1) RandomNumber(1, commonIds.size) else 1

    // 3. draw randomly neuronIds, splitting the set into two
    val (idsToSwitch, _) = Utils.splitIdsRandomly(commonIds, idsToDraw)
    val leftIds = var1Ids.map(id => neuronId(if (idsToSwitch.contains(id)) genome.id else this.id, id))
    val rightIds = var2Ids.map(id => neuronId(if (idsToSwitch.contains(id)) this.id else genome.id, id))

    // 4. create genomes with switch neurons
    val allVariables = var1 ++ var2
    val leftGenome = NetGenome.breed(this, allVariables.filter( n => leftIds.contains(n.id)), trimEnabled, renameEnabled)
    val rightGenome = NetGenome.breed(genome, allVariables.filter( n => rightIds.contains(n.id)), trimEnabled, renameEnabled)
    (leftGenome, rightGenome)
  } else (clone, genome.clone)

  def deleteNeuron(id: String): Unit = {
    debug(this,s"delete neuron $id. was ${_data.neurons.size}")
    _data = _data.withNeurons( neurons.filterNot( _.id == id ) )
    debug(this,s"is ${_data.neurons.size}")
  }

  private def addNeuron(n: NeuronData) = {
    _data = _data.withNeurons(n :: neurons)
  }

  private def updateNeuron(neuronData: NeuronData) = {
    deleteNeuron(neuronData.id)
    addNeuron(neuronData)
  }

  @tailrec
  private def findFirstFreeId(index: Int = 1):Int = find(neuronId(id, index)) match {
    case Some(n) => findFirstFreeId(index + 1)
    case None => index
  }

  private def addNeuron(): Unit ={
    val newId = neuronId(id,findFirstFreeId())
    debug(this,s"adding a neuron to $id -> $newId")
    // 1. create a new neuron with name id+neurons.size and according to NeuronGenome specifications
    val newNG = NeuronGenome.build(newId, accessMap)
    // 2. create exactly one synapse from the set of all neurons (a synapse from an output neuron is also ok)
    val oldNG = NeuronGenome(RandomNumber(neurons), accessMap)
    oldNG.addSynapse(SynapseGenome.build(newNG.id))
    updateNeuron(oldNG.data)
    // 3. create exactly one synapse from the new neuron to the set of not-input neurons
    val mutNs = mutableNeurons()
    if(mutNs.nonEmpty) newNG.addSynapse(SynapseGenome.build(RandomNumber(mutNs).id))

    addNeuron(newNG.data)
  }

  def deleteSynapsesTo(neuronId: String) = {
    debug(this, s"deleting synapses leading to $neuronId")
    val (neuronsToChange, neuronsToLeave) = neurons.partition( _.isConnectedTo(neuronId) )
    val changedNeurons = neuronsToChange.map( n => {
      debug(this, s"deleting synapse ${n.id}->$neuronId")
      val ng = NeuronGenome(n)
      ng.deleteSynapse(neuronId)
      ng.data
    })

    _data = _data.withNeurons(changedNeurons ++ neuronsToLeave)
  }

  private def deleteNeuron(): Unit ={
    val faN = fullAccessNeurons()
    if(faN.nonEmpty){
      val id = RandomNumber(faN).id
      debug(this,s"deleting a neuron from ${data.id} -> $id")
      deleteNeuron(id)
      deleteSynapsesTo(id)
    }
  }

  private def mutateNeuron(): Unit ={
    val mutNs = mutableNeurons()
    if(mutNs.nonEmpty){
      val nCh = NeuronGenome(RandomNumber(mutNs))
      debug(this,s"mutating a neuron in $id -> ${nCh.id}")
      nCh.mutate()
      updateNeuron(nCh.data)
    }
  }

  private def mutateInputTickMultiplier(): Unit ={
    _data = _data.withInputTickMultiplier(RandomNumber(Context().inputTickMultiplierRange))
  }

  def toJson = writePretty(this)
}

object NetGenome {
  def apply(data: NetData, accessMap: Map[String, MutationAccess] = Map()) = new NetGenome(data, accessMap)
  def apply(id: String, neurons: List[NeuronData], inputs: List[String], inputTickMultiplier: Double):NetGenome =
    NetGenome(NetData(id, neurons, inputs, inputTickMultiplier), Map())

  def build(netId: String, inputIds: List[String], outputIds: List[String]) = {
    debug(this, "---------------------------------------------------------------------")
    debug(this, s"building $netId with input ids: $inputIds and output ids: $outputIds")
    debug(this, "")
    
    assert(Context().synapsesDensity >= 1.0, "There should be at least one synapse for neuron")
    assert(inputIds.size + outputIds.size <= Context().neuronsRange.end, s"You chose ${inputIds.size} inputs and ${outputIds.size} outputs, but the max possible neurons number is only ${Context().neuronsRange.end}")

    val neuronsSize = RandomNumber(
      if(inputIds.size + outputIds.size > Context().neuronsRange.start)
        IntRange(inputIds.size + outputIds.size, Context().neuronsRange.end)
      else Context().neuronsRange
    )
    
    debug(this,s"There will be $neuronsSize neurons")

    debug(this,"* building inputs")
    val ins = inputIds.map( NeuronGenome.build(_) )
    debug(this,"* building outputs")
    val outs = outputIds.map( NeuronGenome.build(_) )
    debug(this,"* building middles")
    val middles = (
      for(i <- 1 to neuronsSize - ins.size - outs.size)
        yield NeuronGenome.build(neuronId(netId,i), accessMap(inputIds, outputIds))
    ).toList
    val ns = ins ++ middles ++ outs

    debug(this,"* creating synapses")

    // at least one synapse from each "in" to one of "middles"
    debug(this, "at least one synapse from each \"in\" to one of \"middles\"")
    var synapsesCounter = 0
    ins.foreach(in => chooseNeuron(middles, (n: NeuronGenome) => !in.isConnectedTo(n.id)) match {
      case Some(n) => in.connect(n); synapsesCounter += 1
      case None =>
    })

    // at least one synapse to each "out" from one of "middles"
    debug(this, "at least one synapse to each \"out\" from one of \"middles\"")
    outs.foreach(out => chooseNeuron(middles, (n: NeuronGenome) => !n.isConnectedTo(out.id)) match {
      case Some(n) => n.connect(out); synapsesCounter += 1
      case None =>
    })

    val synapsesSize = Math.round(Context().synapsesDensity * neuronsSize).toInt - synapsesCounter
    debug(this,s"$synapsesCounter created, $synapsesSize left to create")

    if(synapsesSize > 0) {
      val im = ins ++ middles
      val mo = middles ++ outs

      for(i <- 1 to synapsesSize) RandomNumber(im).connect(RandomNumber(mo))
    }
    // @todo: it still doesn't ensure that there is a valid connection from ins to outs

    val ng = NetGenome(
      NetData(netId, ns.map(_.data), inputIds, RandomNumber(Context().inputTickMultiplierRange)),
      accessMap(inputIds, outputIds)
    )

    debug(this, s"done building $netId")
    debug(this, ng.data.toJson)
    debug(this, "---------------------------------------------------------------------")

    ng
  }

  def breed(oldGenome: NetGenome,
            newFullAccess: List[NeuronData],
            trimEnabled: Boolean = true,
            renameEnabled: Boolean = true) = {
    // 6. rename full access neurons so their netId match their new nets
    val newNeurons = oldGenome.notFullAccessNeurons() ++
      (if(renameEnabled) newFullAccess.map( n => {
        n.withSynapses(n.synapses.map( s => s.withId(replaceNetId(s.neuronId, oldGenome.id))))
         .withId(replaceNetId(n.id, oldGenome.id))
      } ) else newFullAccess)
    // 7. compose new NetData
    val newNet = oldGenome.data.withNeurons(newNeurons)
    // 8. "trim" the net, ie. remove synapses which lead to non-existing neurons and full access neurons which do not receive any input
    val newGen = NetGenome(newNet, oldGenome.accessMap)
    if(trimEnabled) newGen.trim()
    if(trimEnabled && renameEnabled) newGen.data.validate()

    debug(this,s"new genome bred: ${newGen.id}")
    debug(this, newGen.data.toJson)
    newGen
  }

  private def chooseNeuron(neurons: List[NeuronGenome], check:(NeuronGenome)=>Boolean):Option[NeuronGenome] = neurons match {
    case Nil => None
    case list => val n = RandomNumber(list)
                 if(check(n)) Some(n)
                 else chooseNeuron(list.filter(_.id != n.id), check)
  }

  def accessMap(inputIds: List[String], outputIds: List[String]) =
    (inputIds.map(_ -> MutationAccessDontMutate()) ++ outputIds.map(_ -> MutationAccessDontDelete())).toMap

  def fromJson(jsonStr: String) = read[NetGenome](jsonStr)
}
