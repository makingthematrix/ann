package anna.epengine

import anna.Context
import anna.data.NetData._
import anna.data._
import anna.logger.LOG._
import anna.utils.Utils.formats
import anna.utils.{IntRange, RandomNumber, Utils}
import org.json4s.native.Serialization.{read, writePretty}
import scala.collection.mutable

import scala.annotation.tailrec

/**
 * Created by gorywoda on 04.01.15.
 */

class NetGenome(var id: String,
                var neurons: mutable.ListBuffer[NeuronGenome],
                val inputs: List[String],
                var threshold: Double,
                var slope: Double,
                var hushValue: HushValue,
                var forgetting: ForgetTrait,
                var tickTimeMultiplier: Double,
                var weight: SynapseTrait,
                var inputTickMultiplier: Double,
                val activationFunctionName: String,
                val accessMap: Map[String, MutationAccess]){
  def find(id: String) = neurons.find( _.id == id )
  def filter(ids: Seq[String]) = neurons.filter(n => ids.contains(n.id))
  def filterNot(ids: Seq[String]) = neurons.filterNot(n => ids.contains(n.id))

  def findSynapse(from: String, to: String) = find(from) match {
    case Some(n) => n.synapses.find( _.neuronId == to )
    case None => None
  }

  def isFullAccess(n: NeuronGenome):Boolean = isFullAccess(n.id)
  def isFullAccess(nid: String):Boolean = accessMap.getOrElse(nid, MutationAccessFull()) == MutationAccessFull()
  def isMutable(n: NeuronGenome):Boolean = isMutable(n.id)
  def isMutable(nid: String):Boolean = accessMap.getOrElse(nid, MutationAccessFull()) != MutationAccessInput()

  def fullAccessNeurons = neurons.filter(isFullAccess).toList
  def mutableNeurons = neurons.filter(isMutable).toList
  def notFullAccessNeurons = neurons.filterNot(isFullAccess).toList

  def netId(newNetId: String) = {
    neurons.foreach(_.synapses.foreach(s => if(isFullAccess(s.neuronId)) s.neuronId = replaceNetId(s.neuronId, newNetId)))
    // sequence is important: if we change ids of neurons before changing synapses, we will never know where the synapses lead to
    fullAccessNeurons.foreach(n => { n.id = replaceNetId(n.id, newNetId)})
    id = newNetId
    this
  }

  def trim():Unit = {
    // identify synapses which lead to non-existing neurons
    // for each such synapse, create a neuron without it and replace the original one with it in data
    val neuronIdSet = neurons.map(_.id).toSet
    neurons.foreach(n => {
      val trimmedSynapses = n.synapses.filter(s => neuronIdSet.contains(s.neuronId))
      if(trimmedSynapses.size != n.synapses.size) n.synapses = trimmedSynapses
    })

    // identify full access neurons which have no synapses leading to them
    // delete these neurons from the data
    val endPointNeuronIds = neurons.map( _.synapses.map(_.neuronId) ).flatten.toSet
    neurons = neurons.filter( n =>
      endPointNeuronIds.contains(n.id) ||
       accessMap.getOrElse(n.id, MutationAccessFull()) != MutationAccessFull()
    )
  }

  def crossable(genome: NetGenome) = {
    val constants1 = notFullAccessNeurons
    val constants2 = genome.notFullAccessNeurons

    assert(
      constants1.map(_.id).toSet == constants2.map(_.id).toSet,
      s"Unable to cross $id with ${genome.id}: The list of constant neurons differ, they are respectively ${constants1.map(_.id).sorted} and ${constants2.map(_.id).sorted}"
    )

    val var1Ids = fullAccessNeurons.map(n => removeNetId(n.id)).toSet
    val var2Ids = genome.fullAccessNeurons.map(n => removeNetId(n.id)).toSet
    // if there is no common part then we cannot cross the genomes
    var1Ids.intersect(var2Ids).nonEmpty
  }

  def crossOnlySynapses(genome: NetGenome) = if(crossable(genome)){
    val n1Map = neurons.map(n => (removeNetId(n.id) -> n)).toMap
    val n2Map = genome.neurons.map(n => (removeNetId(n.id) -> n)).toMap
    val commonIds = n1Map.keySet.intersect(n2Map.keySet)

    val idsToDraw = if (commonIds.size > 1) RandomNumber(1, commonIds.size) else 1
    val (idsToSwitch, _) = Utils.splitIdsRandomly(commonIds, idsToDraw)

    val leftGenome = NetGenome.swapSynapses(this, NetGenome.synapsesMap(n2Map, commonIds), idsToSwitch)
    val rightGenome = NetGenome.swapSynapses(genome, NetGenome.synapsesMap(n1Map, commonIds), idsToSwitch)

    (leftGenome, rightGenome)
  } else (clone, genome.clone)

  def cross(genome: NetGenome) = if(crossable(genome)) {
    //debug(this, s"--- crossing ${this.id} with ${genome.id}")
    val fullAccessN1 = fullAccessNeurons
    //debug(this,s"full access neurons 1: $fullAccessN1")
    val fullAccessN2 = genome.fullAccessNeurons
    //debug(this,s"full access neurons 2: $fullAccessN2")
    // 1. a variable neuron id should be as follows: [netId]_[neuronId]. strip netId and assume that neurons from both
    // nets with the same neuronId are equivalent and so, after the cross they cannot end up in the same new net.
    val fan1Ids = fullAccessN1.map(n => removeNetId(n.id)).toSet
   // debug(this,s"full access ids 1: $fan1Ids")
    val fan2Ids = fullAccessN2.map(n => removeNetId(n.id)).toSet
    //debug(this,s"full access ids 2: $fan2Ids")
    val commonIds = fan1Ids.intersect(fan2Ids)
    //debug(this,s"common ids: $commonIds")
    // if crossable is true it means that there is a common part which we can cross

    // 2. choose the number of neuronIds to draw
    val idsToDraw = if (commonIds.size > 1) RandomNumber(1, commonIds.size) else 1
    //debug(this,s"idsToDraw: $idsToDraw")

    // 3. draw randomly neuronIds, splitting the set into two
    val (idsToSwitch, _) = Utils.splitIdsRandomly(commonIds, idsToDraw)
    //debug(this,s"idsToSwitch: $idsToSwitch")
    val leftIds = fan1Ids.map(id => neuronId(if (idsToSwitch.contains(id)) genome.id else this.id, id))
    //debug(this,s"leftIds: $leftIds")
    val rightIds = fan2Ids.map(id => neuronId(if (idsToSwitch.contains(id)) this.id else genome.id, id))
    //debug(this,s"rightIds: $rightIds")

    // 4. create genomes with switch neurons
    val allVariables = fullAccessN1 ++ fullAccessN2
    val leftGenome = NetGenome.breed(this, allVariables.filter( n => leftIds.contains(n.id)).toList)
    val rightGenome = NetGenome.breed(genome, allVariables.filter( n => rightIds.contains(n.id)).toList)
    (leftGenome, rightGenome)
  } else (clone, genome.clone)

  def deleteNeuron(id: String): Unit = {
    neurons = neurons.filterNot( _.id == id )
  }

  def addNeuron(n: NeuronGenome): Unit = {
    neurons += n
  }

  @tailrec
  final def findFirstFreeId(index: Int = 1):Int = find(neuronId(id, index)) match {
    case Some(n) => findFirstFreeId(index + 1)
    case None => index
  }

  def deleteSynapsesTo(neuronId: String) = {
    debug(this, s"deleting synapses leading to $neuronId")
    neurons.foreach(n => if( n.isConnectedTo(neuronId) ) n.deleteSynapse(neuronId))
  }

  def toJson = writePretty(this)
  def data = NetData(id, neurons.map(_.data).toList, inputs, threshold, slope, hushValue, forgetting,
                     tickTimeMultiplier, weight, inputTickMultiplier, activationFunctionName)
  override def clone = new NetGenome(id, neurons.map(_.clone), inputs, threshold, slope, hushValue, forgetting,
                                     tickTimeMultiplier, weight, inputTickMultiplier, activationFunctionName, accessMap)

  def synapses = neurons.flatMap(_.synapses).toList // mainly for debug purposes
}

object NetGenome {
  def apply(data: NetData, accessMap: Map[String, MutationAccess] = Map()) = {
    val nListBuffer = mutable.ListBuffer[NeuronGenome]()
    nListBuffer ++= data.neurons.map(NeuronGenome(_))
    new NetGenome(data.id, nListBuffer, data.inputs, data.threshold, data.slope, data.hushValue,
                  data.forgetting, data.tickTimeMultiplier, data.weight, data.inputTickMultiplier,
                  data.activationFunctionName, accessMap)
  }

  def apply(id: String, neurons: List[NeuronData], inputs: List[String], inputTickMultiplier: Double):NetGenome =
    NetGenome(NetData(id, neurons, inputs, inputTickMultiplier), Map())

  def build(netId: String, inputIds: List[String], outputIds: List[String]) = {
    debug(this, "---------------------------------------------------------------------")
    debug(this, s"building $netId with input ids: $inputIds and output ids: $outputIds")
    debug(this, "")
    
    assert(Context().synapsesDensity >= 1.0, "There should be at least one synapse for neuron")
    assert(inputIds.size + outputIds.size <= Context().neuronsRange.end, s"You chose ${inputIds.size} inputs and ${outputIds.size} outputs, but the max possible neurons number is only ${Context().neuronsRange.end}")

    val _neuronsSize = RandomNumber(
      if(inputIds.size + outputIds.size > Context().neuronsRange.start)
        IntRange(inputIds.size + outputIds.size, Context().neuronsRange.end)
      else Context().neuronsRange
    )

    // the number of neurons should be at least inputs + outputs + 1
    val neuronsSize = if(_neuronsSize <= inputIds.size + outputIds.size) inputIds.size + outputIds.size + 1 else _neuronsSize
    
    debug(this,s"There will be $neuronsSize neurons")

    debug(this,"* building inputs")
    val ins = inputIds.map( NeuronGenome.build(_) )
    debug(this,"* building outputs")
    val outs = outputIds.map( NeuronGenome.build(_) )
    debug(this,"* building middles")
    val middles = (
      for(i <- 1 to neuronsSize - ins.size - outs.size)
        yield NeuronGenome.build(neuronId(netId,i))
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
    debug(this, ng.toJson)
    debug(this, "---------------------------------------------------------------------")

    ng
  }

  def breed(oldGenome: NetGenome,
            newFullAccess: List[NeuronGenome]) = {
    // 6. rename full access neurons so their netId match their new nets
    val newNeurons = mutable.ListBuffer[NeuronGenome]()
    val fan = newFullAccess.map(_.clone)
    fan.foreach(n => {
      n.synapses.foreach(s => if (oldGenome.isFullAccess(s.neuronId)) s.neuronId = replaceNetId(s.neuronId, oldGenome.id))
      n.id = replaceNetId(n.id, oldGenome.id)
    })
    newNeurons ++= fan

    val nfan = oldGenome.notFullAccessNeurons.map(_.clone)
    nfan.foreach(n =>
      n.synapses.foreach(s => if(oldGenome.isFullAccess(s.neuronId)) s.neuronId = replaceNetId(s.neuronId, oldGenome.id))
    )
    newNeurons ++= nfan

    // 7. compose new NetData
   // val newNet = oldGenome.data.withNeurons(fan ++ nfan)
    // 8. "trim" the net, ie. remove synapses which lead to non-existing neurons and full access neurons which do not receive any input
    val newGen = new NetGenome(
      oldGenome.id,
      newNeurons,
      oldGenome.inputs,
      oldGenome.threshold,
      oldGenome.slope,
      oldGenome.hushValue,
      oldGenome.forgetting,
      oldGenome.tickTimeMultiplier,
      oldGenome.weight,
      oldGenome.inputTickMultiplier,
      oldGenome.activationFunctionName,
      oldGenome.accessMap
    )
    newGen.trim()

    debug(this,s"new genome bred: ${newGen.id}")
    newGen
  }

  private def synapsesMap(neuronsMap: Map[String, NeuronGenome], commonIds: Set[String]) =
    neuronsMap.filterKeys(key => commonIds.contains(key)).map{case (id, n) => {
      n.synapses.map(s => {
        val destId = removeNetId(s.neuronId)
        if(commonIds.contains(destId)) Some(s"$id:$destId" -> s) else None
      }).flatten
    }}.flatten.toMap

  private def swapSynapses(genome: NetGenome, synapsesMap: Map[String, SynapseGenome], idsToSwitch: Set[String]) =
    genome.neurons.foreach(n => if(idsToSwitch.contains(n.id+":")){
      n.synapses.foreach(s => if(idsToSwitch.contains(":"+s.neuronId)) s.weight = synapsesMap(n.id+":"+s.neuronId).weight)
    })


  private def chooseNeuron(neurons: List[NeuronGenome], check:(NeuronGenome)=>Boolean):Option[NeuronGenome] = neurons match {
    case Nil => None
    case list => val n = RandomNumber(list)
                 if(check(n)) Some(n)
                 else chooseNeuron(list.filter(_.id != n.id), check)
  }

  def accessMap(inputIds: List[String], outputIds: List[String]) =
    (inputIds.map(_ -> MutationAccessInput()) ++ outputIds.map(_ -> MutationAccessOutput())).toMap

  def fromJson(jsonStr: String) = read[NetGenome](jsonStr)
}
