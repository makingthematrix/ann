package anna.epengine

import anna.data.{NeuronData, NetData}
import anna.utils.DoubleRange._

/**
 * Created by gorywoda on 04.01.15.
 */

class NetChromosome(private var _data: NetData, val accessMap: Map[String, MutationAccess.Value]){
  def id = _data.id
  def neurons = _data.neurons
  def inputs = _data.inputs
  def inputTickMultiplier = _data.inputTickMultiplier
  def data = _data
  def find(id: String) = _data.neurons.find( _.id == id )

  var addNeuronProbability = Probability(0.1)
  var deleteNeuronProbability = Probability(0.1)
  var mutateNeuronProbability = Probability(0.75)
  var inputTickMultiplierProbability = Probability(0.05)
  var inputTickMultiplierRange = 2.0 <=> 4.0
}

object NetChromosome {
  def apply(data: NetData, accessMap: Map[String, MutationAccess.Value]) = new NetChromosome(data, accessMap)
  def apply(id: String, neurons: List[NeuronData], inputs: List[String], inputTickMultiplier: Double):NetChromosome =
    NetChromosome(NetData(id, neurons, inputs, inputTickMultiplier), Map())
}
