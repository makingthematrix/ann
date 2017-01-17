package anna.data

import anna.Context
import anna.utils.Utils
import anna.utils.Utils.{formats, synapseId}
import org.json4s.native.Serialization.{read, writePretty}

/**
 * Created by gorywoda on 03.01.15.
 */

case class NetData(id: String,
                   neurons: List[NeuronData],
                   inputs: List[String], // time wasted refactoring this to Set[String]: 3h; please update when needed
                   threshold: Double,
                   silenceIterations: SilenceIterationsTrait,
                   weight: SynapseTrait,
                   neuronsInitSilenced: List[String]){
  def withId(id: String) = copy(id = id)
  def withNeurons(neurons: List[NeuronData]) = copy(neurons = neurons)
  def withInputs(inputs: List[String]) = copy(inputs = inputs)
  def withThreshold(threshold: Double) = copy(threshold = threshold)
  def withSilenceIterations(silenceIterations: SilenceIterationsTrait) = copy(silenceIterations = silenceIterations)
  def withWeight(weight: SynapseWeight) = copy(weight = weight)
  def withNeuronsInitSilenced(neuronsInitSilenced: List[String]) = copy(neuronsInitSilenced = neuronsInitSilenced)

  def neuron(neuronId: String) = neurons.find(n => n.id == neuronId || NetData.removeNetId(n.id) == neuronId)
                                        .getOrElse(throw new IllegalArgumentException(s"No neuron found with id $neuronId"))
  def contains(neuronId: String) = neurons.exists(n => n.id == neuronId || NetData.removeNetId(n.id) == neuronId)
  def synapses:Map[String,SynapseData] = neurons.flatMap(n => n.synapses.map(s => (synapseId(n.id, s.neuronId) -> s))).toMap
  def synapse(from: String, to: String) = neurons.find(n => n.id == from || NetData.removeNetId(n.id) == from) match {
    case Some(n) => n.synapses.find(s => s.neuronId == to || NetData.removeNetId(s.neuronId) == to)
    case None => None
  }

  def findIdsConnectedTo(neuronId: String) = neurons.filter(_.synapses.map(_.neuronId).contains(neuronId)).map(_.id)

  def toJson = writePretty(this)

  def filter(ids: Seq[String]) = {
    val idsSet = ids.toSet
    neurons.filter( n => idsSet.contains(n.id) )
  }

  def filterNot(ids: Seq[String]) = {
    val idsSet = ids.toSet
    neurons.filterNot( n => idsSet.contains(n.id) )
  }

  def validate(): Unit = {
    // so, what possibly can go wrong with a net?
    // trivial checks:
    // 1. The net id should not contain '_' as this is how we separate net id from neuron id
    assert(!id.contains("_"), "The net id should not contain '_' as this is how we separate net id from neuron id: " + id)
    // 2. There has to be at least one input neuron
    assert(inputs.nonEmpty, "There has to be at least one input neuron in the net: " + id)
    // 3. And it should be on the neurons list
    inputs.foreach( inId => assert(neurons.find(_.id == inId) != None, s"The input id $inId does not match any of the neurons in the net $id"))
    // 4. Threshold should be >=0 and <1
    assert(threshold >= 0.0 && threshold < 1.0,s"Threshold should be in <0.0,1.0) but is $threshold in the net $id")
    // 5. check if all synapses connect to existing neurons
    val nIdSet = neurons.map(_.id).toSet
    neurons.foreach(n => {
      n.synapses.foreach(s => assert(nIdSet.contains(s.neuronId), s"The synapse from ${n.id} to ${s.neuronId} leads to a neuron which does not exist in the net $id"))
    })

    // 6. one neuron should not have two synapses to the same neuron
    neurons.foreach( n => {
      val neuronIds = n.synapses.map(_.neuronId)
      val neuronIdsSet = neuronIds.toSet
      val doubledIds = neuronIds.filterNot( id => neuronIdsSet.contains(id) )
      assert(doubledIds.isEmpty, s"In the neuron ${n.id}, more than one synapse leads to the same neuron: $doubledIds")
    })

    // @todo: 7. check if there is no neuron with no synapse leading to it
    // disabled until I figure out how to (and if at all) filter out non-accessible output neurons
    // - they can happen, they cannot be deleted, but they don't necessarily are invalid
    //val idLeft = neurons.foldLeft(nIdSet)( (idSet: Set[String], n: NeuronData) => idSet -- n.synapses.map(_.neuronId)) -- inputs
    //assert(idLeft.isEmpty, s"There are neurons with no synapses leading to them: $idLeft in the net $id")
  }
}

object NetData {
  def apply(id: String):NetData = NetData(id, Nil, Nil, Nil)
  def apply(id: String, neurons: List[NeuronData], inputs: List[String], neuronsInitSilenced: List[String]):NetData =
    NetData(id, neurons, inputs, Context().threshold,
            SilenceIterations(Context().silenceIterations), Context().weight, neuronsInitSilenced)

  def fromJson(jsonStr: String) = read[NetData](jsonStr)

  def neuronId(netId: String, id: String): String = s"${netId}_$id"
  def neuronId(netId: String, index: Int): String = s"${netId}_$index"
  def removeNetId(id: String): String = if(id.contains("_")) id.substring(id.indexOf("_")+1) else id

  def replaceNetId(oldNeuronId: String, newNetId: String): String =
    if(oldNeuronId.contains("_")) newNetId + "_" + oldNeuronId.substring(oldNeuronId.indexOf("_")+1)
    else newNetId + "_" + oldNeuronId
}
