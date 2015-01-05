package anna.epengine

import anna.data.{NeuronData, NetData}

/**
 * Created by gorywoda on 04.01.15.
 */
case class NetChromosome(data: NetData){
  lazy val id = data.id
  lazy val neurons = data.neurons
  lazy val inputs = data.inputs
  lazy val inputTickMultiplier = data.inputTickMultiplier
}

object NetChromosome {
  def apply(id: String, neurons: List[NeuronData], inputs: List[String], inputTickMultiplier: Double):NetChromosome =
    NetChromosome(NetData(id, neurons, inputs, inputTickMultiplier))
}
