package anna.epengine

import anna.data.{NeuronData, NetData}
import anna.utils.{Utils, IntRange}
import anna.utils.DoubleRange._
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

  def mutate() = Probability.performRandom(
    (addNeuronProbability, addNeuron _),
    (deleteNeuronProbability, deleteNeuron _),
    (mutateNeuronProbability, mutateNeuron _),
    (inputTickMultiplierProbability, mutateInputTickMultiplier _)
  )

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
    val newNG = NeuronGenome.toss(id + neurons.size, accessMap)
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

  private def fullAccessNeurons() = neurons.filter( n => accessMap.getOrElse(n.id, MutationAccess.FULL) == MutationAccess.FULL)
  private def mutableNeurons() = neurons.filter( n => accessMap.getOrElse(n.id, MutationAccess.FULL) != MutationAccess.DONTMUTATE)
}

object NetGenome {
  var addNeuronProbability = Probability(0.1)
  var deleteNeuronProbability = Probability(0.1)
  var mutateNeuronProbability = Probability(0.75)
  var inputTickMultiplierProbability = Probability(0.05)
  var inputTickMultiplierRange = 2.0 <=> 2.0

  var neuronsRange:Range = 5 to 10
  var synapsesDensity:Double = 2.5

  def apply(data: NetData, accessMap: Map[String, MutationAccess.Value]) = new NetGenome(data, accessMap)
  def apply(id: String, neurons: List[NeuronData], inputs: List[String], inputTickMultiplier: Double):NetGenome =
    NetGenome(NetData(id, neurons, inputs, inputTickMultiplier), Map())

  def toss(netId: String, inputIds: List[String], outputIds: List[String]) = {
    assert(synapsesDensity >= 1.0, "There should be at least one synapse for neuron")
    assert(inputIds.size + outputIds.size <= neuronsRange.end, s"You chose ${inputIds.size} inputs and ${outputIds.size} outputs, but the max possible neurons number is only ${neuronsRange.end}")
    val r = if(inputIds.size + outputIds.size > neuronsRange.start) (inputIds.size + outputIds.size) to neuronsRange.end else neuronsRange
    val neuronsSize = RandomNumber(r)

    val ins = inputIds.map( NeuronGenome.toss(_) )
    val outs = outputIds.map( NeuronGenome.toss(_) )
    val middles = ( for(i <- 1 to neuronsSize - ins.size - outs.size) yield NeuronGenome.toss(netId+"_"+i) ).toList
    val ns = ins ++ middles ++ outs

    // at least one synapse from each "in" to one of "middles"
    var synapsesCounter = 0
    ins.foreach( in => {
      val check:(NeuronGenome)=>Boolean = (n: NeuronGenome) => { !in.isConnectedTo(n.id) }
      val middleOpt = chooseNeuron(middles, check)
      if(middleOpt != None) {
        connect(in, middleOpt.get)
        synapsesCounter += 1
      }
    })

    // at least one synapse to each "out" from one of "middles"
    outs.foreach( out => {
      val check:(NeuronGenome)=>Boolean = (n: NeuronGenome) => { !n.isConnectedTo(out.id) }
      val middleOpt = chooseNeuron(middles, check)
      if(middleOpt != None) {
        connect(middleOpt.get, out)
        synapsesCounter += 1
      }
    })

    val synapsesSize = Math.round(synapsesDensity * neuronsSize).toInt - synapsesCounter

    if(synapsesSize > 0) {
      val im = ins ++ middles
      val mo = middles ++ outs

      for(i <- 1 to synapsesSize) connect(RandomNumber(im), RandomNumber(mo))
    }
    // @todo: it still doesn't ensure that there is a valid connection from ins to outs

    val inputTickMultiplier = RandomNumber(inputTickMultiplierRange)

    val netData = NetData(netId, ns.map(_.data), inputIds, inputTickMultiplier)
    val accessMap = (inputIds.map(_ -> MutationAccess.DONTMUTATE) ++ outputIds.map(_ -> MutationAccess.DONTDELETE)).toMap
    NetGenome(netData, accessMap)
  }

  def cross(gen1: NetGenome, gen2: NetGenome):(NetGenome, NetGenome) = {
    val constants1 = gen1.neurons.filter(n => gen1.accessMap.getOrElse(n.id, MutationAccess.FULL) != MutationAccess.FULL)
    val constants2 = gen2.neurons.filter(n => gen1.accessMap.getOrElse(n.id, MutationAccess.FULL) != MutationAccess.FULL)
    assert(constants1.map(_.id).toSet == constants2.map(_.id).toSet, s"Unable to cross ${gen1.id} with ${gen2.id}: The list of constant neurons differ, they are respectively ${constants1.map(_.id).sorted} and ${constants2.map(_.id).sorted}")

    if (constants1.size == gen1.neurons.size && constants2.size == gen2.neurons.size)
      (gen1, gen2)
    else
      cross_p(gen1, gen2)
  }

  private def splitIdsRandomly(oldList: List[String], idsToDraw: Int, newList: List[String] = Nil):(List[String],List[String]) = idsToDraw match {
    case 0 => (newList, oldList)
    case n =>
      val id = RandomNumber.apply(oldList)
      (id :: newList, oldList - id)
  }

  private def cross_p(gen1: NetGenome, gen2: NetGenome):(NetGenome, NetGenome) = {
    val variables1 = gen1.neurons.filter(n => gen1.accessMap.getOrElse(n.id, MutationAccess.FULL) == MutationAccess.FULL)
    val variables2 = gen2.neurons.filter(n => gen1.accessMap.getOrElse(n.id, MutationAccess.FULL) == MutationAccess.FULL)

    // 1. a variable neuron id should be as follows: [netId]_[neuronId]. strip netId and assume that neurons from both
    // nets with the same neuronId are equivalent and so, after the cross they cannot end up in the same new net.
    // 2. create a set of unique neuronIds
    val neuronIds = (variables1.map(n => if(n.id.startsWith(gen1.id)) n.id.substring(gen1.id.length) else n.id) ++
                     variables2.map(n => if(n.id.startsWith(gen2.id)) n.id.substring(gen2.id.length) else n.id)).toSet

    // 3. choose the number of neuronIds to draw from that set: it's between 1 and the size of the smaller net - 1
    val minPart = Math.min(variables1.size, variables2.size) - 1
    val idsToDraw = if(minPart <= 1) 1 else RandomNumber(1, minPart)
    // 4. draw randomly neuronIds, splitting the set into two
    // 5. constants + neurons with ids matching the first set is the new net1, constants + neurons with ids matching the second set is the new net2
    // 6. rename the variable neurons so their netId match their new nets

    (gen1, gen2)
  }



  private def connect(from: NeuronGenome, to:NeuronGenome): Boolean = if(from.isConnectedTo(to.id)) false else {
    from.addSynapse(SynapseGenome.toss(to.id))
    true
  }

  private def chooseNeuron(neurons: List[NeuronGenome], check:(NeuronGenome)=>Boolean):Option[NeuronGenome] = neurons match {
    case Nil => None
    case list => val n = RandomNumber(list)
      if(check(n)) Some(n) else chooseNeuron(list.filter(_.id != n.id), check)
  }

}
