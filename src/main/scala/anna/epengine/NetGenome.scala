package anna.epengine

import anna.data.{SynapseData, NeuronData, NetData}
import anna.utils.{RandomNumber, Utils}
import anna.utils.DoubleRange._
import anna.data.NetData.neuronId
import anna.data.NetData.removeNetId
import anna.data.NetData.replaceNetId

import anna.logger.LOG._

/**
 * Created by gorywoda on 04.01.15.
 */

object MutationAccess extends Enumeration {
  type MutationAccess = Value
  val FULL, DONTDELETE, DONTMUTATE = Value
}

class NetGenome(private var _data: NetData, val accessMap: Map[String, MutationAccess.Value]){
  import NetGenome._

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

  def fullAccessNeurons() = neurons.filter(n => accessMap.getOrElse(n.id, MutationAccess.FULL) == MutationAccess.FULL)
  def mutableNeurons() = neurons.filter( n => accessMap.getOrElse(n.id, MutationAccess.FULL) != MutationAccess.DONTMUTATE)
  def notFullAccessNeurons() = neurons.filterNot(n => accessMap.getOrElse(n.id, MutationAccess.FULL) == MutationAccess.FULL)

  def mutate() = Probability.performRandom(
    (addNeuronProbability, addNeuron _),
    (deleteNeuronProbability, deleteNeuron _),
    (mutateNeuronProbability, mutateNeuron _),
    (inputTickMultiplierProbability, mutateInputTickMultiplier _)
  )

  def trim():Unit = {
    // identify synapses which lead to non-existing neurons
    // for each such synapse, create a neuron without it and replace the original one with it in data
    val neuronIdSet = neurons.map(_.id).toSet
    debug(this,s"neuronIdSet: $neuronIdSet")
    val t1 = neurons.map(n => n.synapses.find(s => !neuronIdSet.contains(s.neuronId)) match {
      case Some(synapse) => Some((n.id, synapse.neuronId))
      case None => None
    }).flatten
    debug(this,s"t1: $t1")
    t1.foreach( t => replaceSynapses(t._1, find(t._1).get.synapses.filterNot( _.neuronId == t._2 )) )
    // identify full access neurons which have no synapses leading to them
    // delete these neurons from the data
    val endPointNeuronIds = neurons.map( _.synapses.map(_.neuronId) ).flatten.toSet
    neurons.filter( n =>
      !endPointNeuronIds.contains(n.id) &&
       accessMap.getOrElse(n.id, MutationAccess.FULL) == MutationAccess.FULL
    ).foreach( n => deleteNeuron(n.id) )
  }

  private def replaceSynapses(neuronId: String, synapses: List[SynapseData]): Unit = {
    _data = _data.neurons.find(_.id == neuronId) match {
      case Some(neuron) => _data.withNeurons(neuron.withSynapses(synapses) :: _data.neurons.filterNot(_.id == neuronId))
      case None => _data
    }
  }

  private def deleteNeuron(id: String): Unit = {
    _data = _data.withNeurons( neurons.filterNot( _.id == id ) )
  }

  private def addNeuron(n: NeuronData) = {
    _data = _data.withNeurons(n :: neurons)
  }

  private def updateNeuron(neuronData: NeuronData) = {
    deleteNeuron(neuronData.id)
    addNeuron(neuronData)
  }

  private def addNeuron(): Unit ={
    // 1. create a new neuron with name id+neurons.size and according to NeuronGenome specifications
    val newNG = NeuronGenome.toss(neuronId(id,neurons.size), accessMap)
    // 2. create exactly one synapse from the set of all neurons (a synapse from an output neuron is also ok)
    val oldNG = NeuronGenome(RandomNumber(neurons), accessMap)
    oldNG.addSynapse(SynapseGenome.toss(newNG.id))
    updateNeuron(oldNG.data)
    // 3. create exactly one synapse from the new neuron to the set of not-input neurons
    val mutNs = mutableNeurons()
    if(mutNs.nonEmpty) newNG.addSynapse(SynapseGenome.toss(RandomNumber(mutNs).id))

    addNeuron(newNG.data)
  }

  private def deleteSynapsesTo(neuronId: String) = neurons.map( n => if(n.isConnectedTo(id)){
    val nCh = NeuronGenome(n)
    nCh.deleteSynapse(id)
    Some(nCh.data)
  } else None).flatten.foreach( n => updateNeuron(n) )

  private def deleteNeuron(): Unit ={
    val faN = fullAccessNeurons()
    if(faN.nonEmpty){
      val id = RandomNumber(faN).id
      deleteNeuron(id)
      deleteSynapsesTo(id)
    }
  }

  private def mutateNeuron(): Unit ={
    val mutNs = mutableNeurons()
    if(mutNs.nonEmpty){
      val nCh = NeuronGenome(RandomNumber(mutNs))
      nCh.mutate()
      updateNeuron(nCh.data)
    }
  }

  private def mutateInputTickMultiplier(): Unit ={
    _data = _data.withInputTickMultiplier(RandomNumber(inputTickMultiplierRange))
  }

}

object NetGenome {
  var addNeuronProbability = Probability(0.1)
  var deleteNeuronProbability = Probability(0.1)
  var mutateNeuronProbability = Probability(0.75)
  var inputTickMultiplierProbability = Probability(0.05)
  var inputTickMultiplierRange = 2.0 <=> 2.0

  var neuronsRange:Range = 5 to 10
  var synapsesDensity:Double = 2.5

  def apply(data: NetData, accessMap: Map[String, MutationAccess.Value] = Map()) = new NetGenome(data, accessMap)
  def apply(id: String, neurons: List[NeuronData], inputs: List[String], inputTickMultiplier: Double):NetGenome =
    NetGenome(NetData(id, neurons, inputs, inputTickMultiplier), Map())

  def toss(netId: String, inputIds: List[String], outputIds: List[String]) = {
    assert(synapsesDensity >= 1.0, "There should be at least one synapse for neuron")
    assert(inputIds.size + outputIds.size <= neuronsRange.end, s"You chose ${inputIds.size} inputs and ${outputIds.size} outputs, but the max possible neurons number is only ${neuronsRange.end}")

    val accessMap = (inputIds.map(_ -> MutationAccess.DONTMUTATE) ++ outputIds.map(_ -> MutationAccess.DONTDELETE)).toMap

    val r = if(inputIds.size + outputIds.size > neuronsRange.start) (inputIds.size + outputIds.size) to neuronsRange.end
            else neuronsRange
    val neuronsSize = RandomNumber(r)

    val ins = inputIds.map( NeuronGenome.toss(_) )
    val outs = outputIds.map( NeuronGenome.toss(_) )
    val middles = ( for(i <- 1 to neuronsSize - ins.size - outs.size) yield NeuronGenome.toss(neuronId(netId,i), accessMap)).toList
    val ns = ins ++ middles ++ outs

    // at least one synapse from each "in" to one of "middles"
    var synapsesCounter = 0
    ins.foreach( in => {
      def check(n: NeuronGenome) = !in.isConnectedTo(n.id)
      val middleOpt = chooseNeuron(middles, check _)
      if(middleOpt != None) {
        in.connect(middleOpt.get)
        synapsesCounter += 1
      }
    })

    // at least one synapse to each "out" from one of "middles"
    outs.foreach( out => {
      def check(n: NeuronGenome) = !n.isConnectedTo(out.id)
      val middleOpt = chooseNeuron(middles, check _)
      if(middleOpt != None) {
        middleOpt.get.connect(out)
        synapsesCounter += 1
      }
    })

    val synapsesSize = Math.round(synapsesDensity * neuronsSize).toInt - synapsesCounter

    if(synapsesSize > 0) {
      val im = ins ++ middles
      val mo = middles ++ outs

      for(i <- 1 to synapsesSize) RandomNumber(im).connect(RandomNumber(mo))
    }
    // @todo: it still doesn't ensure that there is a valid connection from ins to outs

    val inputTickMultiplier = RandomNumber(inputTickMultiplierRange)

    val netData = NetData(netId, ns.map(_.data), inputIds, inputTickMultiplier)
    NetGenome(netData, accessMap)
  }

  def cross(gen1: NetGenome, gen2: NetGenome, trimEnabled: Boolean = true, renameEnabled: Boolean = true) = {
    val constants1 = gen1.notFullAccessNeurons()
    debug(this,s"contants in gen1: ${constants1.size}")
    val constants2 = gen2.notFullAccessNeurons()
    debug(this,s"contants in gen2: ${constants2.size}")
    assert(constants1.map(_.id).toSet == constants2.map(_.id).toSet, s"Unable to cross ${gen1.id} with ${gen2.id}: The list of constant neurons differ, they are respectively ${constants1.map(_.id).sorted} and ${constants2.map(_.id).sorted}")

    if (constants1.size == gen1.neurons.size && constants2.size == gen2.neurons.size)
      (gen1, gen2)
    else
      cross_p(gen1, gen2, trimEnabled, renameEnabled)
  }

  def createNewGenome(oldGenome: NetGenome,
                      variables: List[NeuronData],
                      chosenIds: Set[String],
                      trimEnabled: Boolean = true,
                      renameEnabled: Boolean = true) = {
    debug(this, s"creating new genome from ${oldGenome.id} with variables ${variables.size}, chosenIds: $chosenIds, trim enabled: $trimEnabled, and renameEnabled: $renameEnabled")
    // 5. constants + neurons with ids matching the first set is the new net1, constants + neurons with ids matching the second set is the new net2
    debug(this,s"all variables: " + variables.map(_.id))

    val newVariables = variables.filter( n => chosenIds.contains(n.id) )
    debug(this,s"new variables: " + newVariables.map(_.id))
    // 6. rename full access neurons so their netId match their new nets
    val newNeurons = oldGenome.notFullAccessNeurons() ++
      (if(renameEnabled) newVariables.map( n => n.withId(replaceNetId(n.id, oldGenome.id)) ) else newVariables)
    debug(this,"new neurons: " + newNeurons.map(_.id))
    debug(this,"old genome access map: " + oldGenome.accessMap)
    // 7. compose new NetData
    val newNet = oldGenome.data.withNeurons(newNeurons)
    // 8. "trim" the net, ie. remove synapses which lead to non-existing neurons and full access neurons which do not receive any input
    val newGen = NetGenome(newNet, oldGenome.accessMap)
    debug(this,"new gen ids before trim: " + newGen.neurons.map(_.id))
    if(trimEnabled) newGen.trim()
    debug(this,"new gen ids after trim: " + newGen.neurons.map(_.id))
    newGen
  }

  private def cross_p(gen1: NetGenome, gen2: NetGenome, trimEnabled: Boolean, renameEnabled: Boolean):(NetGenome, NetGenome) = {
    val variables1 = gen1.fullAccessNeurons()
    val var1Ids = variables1.map(_.id).toSet
    debug(this,s"full access neurons for gen1: ${variables1.size}")
    val variables2 = gen2.fullAccessNeurons()
    val var2Ids = variables2.map(_.id).toSet
    debug(this,s"full access neurons for gen2: ${variables2.size}")
    val allVariables = variables1 ++ variables2

    debug(this,s"all variables: ${allVariables.size}")
    val netIds = List(gen1.id, gen2.id)
    debug(this,s"net ids: $netIds")
    // 1. a variable neuron id should be as follows: [netId]_[neuronId]. strip netId and assume that neurons from both
    // nets with the same neuronId are equivalent and so, after the cross they cannot end up in the same new net.
    // 2. create a set of unique neuronIds
    val neuronIds = allVariables.map(n => removeNetId(n.id)).toSet
    debug(this,s"variable neuron ids: $neuronIds")
    // 3. choose the number of neuronIds to draw from that set: it's between 1 and the size of the smaller net - 1
    val minPart = Math.min(variables1.size, variables2.size) - 1
    debug(this,s"min part: $minPart")
    val idsToDraw = if(minPart <= 1) 1 else RandomNumber(1, minPart)
    debug(this,s"ids to draw: $idsToDraw")
    // 4. draw randomly neuronIds, splitting the set into two
    val (l, r) = Utils.splitIdsRandomly(neuronIds, idsToDraw)
    val leftIds = l.map(id => if(var1Ids.contains(gen1.id+"_"+id)) gen1.id+"_"+id else gen2.id+"_"+id).toSet
    val rightIds = allVariables.map(_.id).filterNot(leftIds.contains(_)).toSet
    debug(this,s"left ids: $leftIds")
    val leftGenome = createNewGenome(gen1, allVariables, leftIds, trimEnabled, renameEnabled)
    debug(this,s"right ids: $rightIds")
    val rightGenome = createNewGenome(gen2, allVariables, rightIds, trimEnabled, renameEnabled)
    (leftGenome, rightGenome)
  }

  private def chooseNeuron(neurons: List[NeuronGenome], check:(NeuronGenome)=>Boolean):Option[NeuronGenome] = neurons match {
    case Nil => None
    case list => val n = RandomNumber(list)
                 if(check(n)) Some(n)
                 else chooseNeuron(list.filter(_.id != n.id), check)
  }

}
