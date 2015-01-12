package anna.epengine

import anna.data.{NeuronData, NetData}
import anna.utils.DoubleRange._

/**
 * Created by gorywoda on 04.01.15.
 */

object MutationAccess extends Enumeration {
  type MutationAccess = Value
  val FULL, DONTDELETE, NONE = Value
}

class NetChromosome(private var data: NetData, private var accessMap: Map[String, MutationAccess.Value]){
  def id = data.id
  def neurons = data.neurons
  def inputs = data.inputs
  def inputTickMultiplier = data.inputTickMultiplier
  def net = data
  def find(id: String) = data.neurons.find( _.id == id )

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
