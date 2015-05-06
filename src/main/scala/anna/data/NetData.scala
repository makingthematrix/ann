package anna.data

import anna.Context
import anna.data.NetData._
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

/**
 * Created by gorywoda on 03.01.15.
 */
case class NetData(id: String,
                   neurons: List[NeuronData],
                   inputs: List[String],
                   threshold: Double,
                   slope: Double,
                   hushValue: HushValue,
                   forgetting: ForgetTrait,
                   tickTimeMultiplier: Double,
                   weight: SynapseTrait,
                   inputTickMultiplier: Double){
  def withId(id: String) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, inputTickMultiplier)
  def withNeurons(neurons: List[NeuronData]) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, inputTickMultiplier)
  def withInputs(inputs: List[String]) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, inputTickMultiplier)
  def withThreshold(threshold: Double) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, inputTickMultiplier)
  def withSlope(slope: Double) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, inputTickMultiplier)
  def withHushValue(hushValue: HushValue) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, inputTickMultiplier)
  def withForgetting(forgetting: ForgetTrait) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, inputTickMultiplier)
  def withTickTimeMultiplier(tickTimeMultiplier: Double) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight,  inputTickMultiplier)
  def withWeight(weight: SynapseWeight) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, inputTickMultiplier)
  def withInputTickMultiplier(inputTickMultiplier: Double) = NetData(id, neurons, inputs, threshold, slope, hushValue, forgetting, tickTimeMultiplier, weight, inputTickMultiplier)

  def toJson = writePretty(this)

  def filter(ids: Seq[String]) = {
    val idsSet = ids.toSet
    neurons.filter( n => idsSet.contains(n.id) )
  }

  def filterNot(ids: Seq[String]) = {
    val idsSet = ids.toSet
    neurons.filterNot( n => idsSet.contains(n.id) )
  }

  def withNewNetId(newNetId: String) = withNeurons(neurons.map(n =>
                                         n.withSynapses(n.synapses.map( s => s.withId(replaceNetId(s.neuronId, newNetId))))
                                          .withId(replaceNetId(n.id, newNetId))
                                       )).withId(newNetId)

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
    // 5. Slope should be >= 1
    assert(slope >= 1.0, s"Slope should be >= 1 but is $slope in the net $id")
    // 6. tick time multiplier should be > 0
    assert(tickTimeMultiplier > 0.0, s"Tick time multiplier should be bigger than 0, is $tickTimeMultiplier in the net $id")
    // 7. input tick multiplier should be > 0
    assert(inputTickMultiplier > 0.0, s"inputTick multiplier should be bigger than 0, is $inputTickMultiplier in the net $id")

    // 8. check if all synapses connect to existing neurons
    val nIdSet = neurons.map(_.id).toSet
    neurons.foreach(n => {
      n.synapses.foreach(s => assert(nIdSet.contains(s.neuronId), s"The synapse from ${n.id} to ${s.neuronId} leads to a neuron which does not exist in the net $id"))
    })

    // 9. one neuron should not have two synapses to the same neuron
    neurons.foreach( n => {
      val neuronIds = n.synapses.map(_.neuronId)
      val neuronIdsSet = neuronIds.toSet
      val doubledIds = neuronIds.filterNot( id => neuronIdsSet.contains(id) )
      assert(doubledIds.isEmpty, s"In the neuron ${n.id}, more than one synapse leads to the same neuron: $doubledIds")
    })

    // 10. check if there is no neuron with no synapse leading to it
    // disabled until I figure out how to (and if at all) filter out non-accessible output neurons
    // - they can happen, they cannot be deleted, but they don't necessarily are invalid
    //val idLeft = neurons.foldLeft(nIdSet)( (idSet: Set[String], n: NeuronData) => idSet -- n.synapses.map(_.neuronId)) -- inputs
    //assert(idLeft.isEmpty, s"There are neurons with no synapses leading to them: $idLeft in the net $id")
  }
}

object NetData {
  def apply(id: String):NetData = NetData(id, Nil, Nil)
  def apply(id: String, neurons: List[NeuronData], inputs: List[String]):NetData =
    NetData(id, neurons, inputs, Context().threshold, Context().slope,
            Context().hushValue, Context().forgetting, 1.0, Context().weight, 1.0)
  def apply(id: String, neurons: List[NeuronData], inputs: List[String], inputTickMultiplier: Double):NetData =
    NetData(id, neurons, inputs, Context().threshold, Context().slope,
            Context().hushValue, Context().forgetting, 1.0, Context().weight,
            inputTickMultiplier)

  def fromJson(jsonStr: String) = read[NetData](jsonStr)

  def neuronId(netId: String, id: String): String = s"${netId}_$id"
  def neuronId(netId: String, index: Int): String = s"${netId}_$index"
  def removeNetId(id: String): String = if(id.contains("_")) id.substring(id.indexOf("_")+1) else id

  def replaceNetId(oldNeuronId: String, newNetId: String): String =
    if(oldNeuronId.contains("_")) newNetId + "_" + oldNeuronId.substring(oldNeuronId.indexOf("_")+1)
    else oldNeuronId
}
