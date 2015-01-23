package anna.epengine

import anna.data.{SynapseData, NeuronData, NetData}
import anna.utils.DoubleRange._

/**
 * Created by gorywoda on 04.01.15.
 */

class NetChromosome(private var _data: NetData, val accessMap: Map[String, MutationAccess.Value]){
  import NetChromosome._

  override def clone = NetChromosome(_data, accessMap)

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

  private def addNeuron(): Unit ={
    // 1. create a new neuron with name id+neurons.size and according to NeuronChromosome specifications
    // 2. create exactly one synapse from the set of all neurons (a synapse from an output neuron is also ok)
    // 3. create exactly one synapse from the new neuron to the set of not-input neurons
  }

  private def deleteNeuron(){}

  private def mutateNeuron(){}

  private def mutateInputTickMultiplier(): Unit ={
    _data = _data.withInputTickMultiplier(inputTickMultiplierRange.choose(RandomNumber()))
  }
}

object NetChromosome {
  var addNeuronProbability = Probability(0.1)
  var deleteNeuronProbability = Probability(0.1)
  var mutateNeuronProbability = Probability(0.75)
  var inputTickMultiplierProbability = Probability(0.05)
  var inputTickMultiplierRange = 2.0 <=> 4.0

  def apply(data: NetData, accessMap: Map[String, MutationAccess.Value]) = new NetChromosome(data, accessMap)
  def apply(id: String, neurons: List[NeuronData], inputs: List[String], inputTickMultiplier: Double):NetChromosome =
    NetChromosome(NetData(id, neurons, inputs, inputTickMultiplier), Map())
}
