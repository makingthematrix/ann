package anna.epengine

import anna.Context
import anna.data.NetData
import anna.logger.LOG._
import anna.utils.RandomNumber
import scala.collection.mutable

/**
 * Created by gorywoda on 11/24/15.
 */

trait SynapseChooser {
  def choose(neuron: NeuronGenome): SynapseGenome
}

trait NeuronChooser {
  def choose(net: NetGenome): NeuronGenome
}

object MutationsLibrary {
  type Mutation = (NetGenome) => Unit

  // this way we can override the choosers for sake of tests and some strange engines
  private var synapseChooser = new SynapseChooser {
    override def choose(neuron: NeuronGenome): SynapseGenome = SynapseGenome(RandomNumber(neuron.synapses))
  }

  private var neuronChooser = new NeuronChooser {
    override def choose(net: NetGenome): NeuronGenome = NeuronGenome(RandomNumber(net.neurons), net.accessMap)
  }

  def set(synapseChooser: SynapseChooser): Unit ={
    MutationsLibrary.synapseChooser = synapseChooser
  }

  def set(neuronChooser: NeuronChooser): Unit ={
    MutationsLibrary.neuronChooser = neuronChooser
  }

  def chooseNeuron(net: NetGenome):NeuronGenome = neuronChooser.choose(net)
  def chooseSynapse(neuron: NeuronGenome):SynapseGenome = synapseChooser.choose(neuron)
  def chooseSynapse(net: NetGenome):SynapseGenome = chooseSynapse(chooseNeuron(net))

  private val mutationsMap = mutable.Map[String, Mutation]()

  def get(name: String) = mutationsMap(name)

  def contains(name: String) = mutationsMap.contains(name)

  def add(name: String, mutation: Mutation):Unit = if(mutationsMap.contains(name)){
    throw new IllegalArgumentException(s"There is already a mutation with the name $name in the MutationsLibrary")
  } else mutationsMap += (name -> mutation)

  private def add(name: String, probability: Probability)(mutation: Mutation):Unit = add(name, mutation)

  add("addNeuron", Context().addNeuronProbability)((net: NetGenome) => {
    val newId = NetData.neuronId(net.id,net.findFirstFreeId())
    debug(s"MUTATION: addNeuron to ${net.id} -> the new neuron's id is $newId")
    // 1. create a new neuron with name id+neurons.size and according to NeuronGenome specifications
    val newNG = NeuronGenome.build(newId, net.accessMap)
    // 2. create exactly one synapse from the set of all neurons (a synapse from an output neuron is also ok)
    val oldNG = chooseNeuron(net)
    oldNG.addSynapse(SynapseGenome.build(newNG.id))
    net.updateNeuron(oldNG.data)
    // 3. create exactly one synapse from the new neuron to the set of not-input neurons
    val mutNs = net.mutableNeurons
    if(mutNs.nonEmpty) newNG.addSynapse(SynapseGenome.build(RandomNumber(mutNs).id))

    net.addNeuron(newNG.data)
  })
}
