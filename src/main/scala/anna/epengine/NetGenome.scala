package anna.epengine

import anna.data.{NeuronData, NetData}
import anna.utils.IntRange
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
    debug(this, "addNeuron, current size of neurons is: " + neurons.size)
    // 1. create a new neuron with name id+neurons.size and according to NeuronGenome specifications
    val newNG = NeuronGenome.toss(id + neurons.size, accessMap)
    debug(this,s"a new neuron created with id ${newNG.id}")
    // 2. create exactly one synapse from the set of all neurons (a synapse from an output neuron is also ok)

    val oldNG = NeuronGenome(neurons(RandomNumber(neurons.size)), accessMap)
    debug(this,s"connecting ${oldNG.id} with the new neuron")
    oldNG.addSynapse(SynapseGenome.toss(newNG.id))
    updateNeuron(oldNG.data)
    // 3. create exactly one synapse from the new neuron to the set of not-input neurons
    val mutNs = mutableNeurons
    if(mutNs.nonEmpty) {
      val connectToId = mutNs(RandomNumber(mutNs.size)).id
      newNG.addSynapse(SynapseGenome.toss(connectToId))
      debug(this, s"connecting the new neuron to $connectToId")
    }

    addNeuron(newNG.data)
    debug(this,"the new neuron was added to the net, the number of neurons is now: " + neurons.size)
  }

  private def deleteSynapsesTo(neuronId: String) = neurons.map( n => if(n.isConnectedTo(id)){
    val nCh = NeuronGenome(n)
    nCh.deleteSynapse(id)
    Some(nCh.data)
  } else None).flatten.foreach( n => updateNeuron(n) )

  private def deleteNeuron(): Unit ={
    val faN = fullAccessNeurons
    if(faN.nonEmpty){
      val id = faN(RandomNumber(faN.size)).id
      deleteNeuron(id)
      deleteSynapsesTo(id)
    }
  }

  private def mutateNeuron(): Unit ={
    val mutNs = mutableNeurons
    if(mutNs.nonEmpty){
      val nCh = NeuronGenome(mutNs(RandomNumber(mutNs.size)))
      nCh.mutate()
      updateNeuron(nCh.data)
    }
  }

  private def mutateInputTickMultiplier(): Unit ={
    _data = _data.withInputTickMultiplier(inputTickMultiplierRange.choose(RandomNumber()))
  }

  private def fullAccessNeurons = neurons.filter( n => accessMap.getOrElse(n.id, MutationAccess.FULL) == MutationAccess.FULL)
  private def mutableNeurons = neurons.filter( n => accessMap.getOrElse(n.id, MutationAccess.FULL) != MutationAccess.DONTMUTATE)
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
    val neuronsSize = r.choose(RandomNumber())

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

      var i = 0
      while(i < synapsesSize) {
        val imIndex = (0 until im.size).choose(RandomNumber())
        val imNeuronChromosome = im(imIndex)
        val moIndex = (0 until mo.size).choose(RandomNumber())
        val moNeuronChromosome = mo(moIndex)
        connect(imNeuronChromosome, moNeuronChromosome)
        i += 1
      }
    }
    // @todo: it still doesn't ensure that there is a valid connection from ins to outs

    val inputTickMultiplier = inputTickMultiplierRange.choose(RandomNumber())

    val netData = NetData(netId, ns.map(_.data), inputIds, inputTickMultiplier)
    val accessMap = (inputIds.map(_ -> MutationAccess.DONTMUTATE) ++ outputIds.map(_ -> MutationAccess.DONTDELETE)).toMap
    NetGenome(netData, accessMap)
  }

  private def connect(from: NeuronGenome, to:NeuronGenome): Boolean = if(from.isConnectedTo(to.id)) false else {
    val synapseChromosome = SynapseGenome.toss(to.id)
    from.addSynapse(synapseChromosome)
    true
  }

  private def chooseNeuron(neurons: List[NeuronGenome], check:(NeuronGenome)=>Boolean):Option[NeuronGenome] = neurons match {
    case Nil => None
    case list => val n = list(RandomNumber(list.size))
      if(check(n)) Some(n) else chooseNeuron(list.filter(_.id != n.id), check)
  }

  implicit private def fromRange(r: Range):IntRange = IntRange(r)
}
