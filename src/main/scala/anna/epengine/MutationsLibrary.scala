package anna.epengine

import anna.Context
import anna.data._
import anna.logger.LOG._
import anna.utils.RandomNumber
import scala.collection.mutable

/**
 * Created by gorywoda on 11/24/15.
 */

trait SynapseChooser {
  def choose(net: NetGenome): Option[(SynapseGenome, NeuronGenome)]
}

trait NeuronChooser {
  def chooseForMutation(net: NetGenome): Option[NeuronGenome]
  def chooseForDeletion(net: NetGenome): Option[NeuronGenome]
}

object MutationsLibrary {
  type Mutation = (NetGenome) => Unit

  // this way we can override the choosers for sake of tests and some strange engines
  private var synapseChooser = new SynapseChooser {
    override def choose(net: NetGenome): Option[(SynapseGenome, NeuronGenome)] = net.neurons.filter(_.synapses.nonEmpty).toList match {
      case Nil =>
        debug(s"Unable to find a neuron with a synapse in ${net.id}")
        None
      case neurons =>
        val n = RandomNumber(neurons)
        val s = SynapseGenome(RandomNumber(n.synapses))
        Some(s, n)
    }
  }

  private var neuronChooser = new NeuronChooser {
    override def chooseForMutation(net: NetGenome) = net.mutableNeurons match {
      case Nil =>
        debug(s"Unable to find a mutable neuron in ${net.id}")
        None
      case neurons => Some(RandomNumber(neurons))
    }
    override def chooseForDeletion(net: NetGenome) = net.fullAccessNeurons match {
      case Nil =>
        debug(s"Unable to find a deletable neuron in ${net.id}")
        None
      case neurons => Some(RandomNumber(neurons))
    }
  }

  def set(synapseChooser: SynapseChooser): Unit ={
    MutationsLibrary.synapseChooser = synapseChooser
  }

  def set(neuronChooser: NeuronChooser): Unit ={
    MutationsLibrary.neuronChooser = neuronChooser
  }

  private def chooseNeuron(net: NetGenome, forDeletion: Boolean = false) =
    if(forDeletion) neuronChooser.chooseForDeletion(net) else neuronChooser.chooseForDeletion(net)
  private def chooseSynapse(net: NetGenome) = synapseChooser.choose(net)

  private val mutationsMap = mutable.Map[String, Mutation]()

  def get(name: String) = mutationsMap(name)

  def contains(name: String) = mutationsMap.contains(name)

  def add(name: String, mutation: Mutation):Unit = if(mutationsMap.contains(name)){
    throw new IllegalArgumentException(s"There is already a mutation with the name $name in the MutationsLibrary")
  } else mutationsMap += (name -> mutation)

  add("addNeuron", (net: NetGenome) => {
    val newId = NetData.neuronId(net.id, net.findFirstFreeId())
    debug(s"MUTATION: addNeuron to ${net.id} -> the new neuron's id is $newId")
    // 1. create a new neuron with name id+neurons.size and according to NeuronGenome specifications
    val newNG = NeuronGenome.build(newId)
    // 2. create exactly one synapse from the set of all neurons (a synapse from an output neuron is also ok)
    RandomNumber(net.neurons).addSynapse(SynapseGenome.build(newNG.id))

    // 3. create exactly one synapse from the new neuron to the set of not-input neurons
    chooseNeuron(net) match {
      case Some(n) => newNG.connect(n)
      case None => debug(s"Unable to find a non-input neuron to connect ${newNG.id} to")
    }

    net.addNeuron(newNG)
  })

  add("deleteNeuron", (net: NetGenome) => chooseNeuron(net, true) match {
    case Some(n) =>
      debug(s"MUTATION: deleteNeuron from ${net.id} -> the deleted neuron's id is ${n.id}")
      net.deleteNeuron(n.id)
      net.deleteSynapsesTo(n.id)
    case None =>
  })

  add("mutateInputTickMultiplier", (net: NetGenome) => {
      val newInputTickMultiplier = RandomNumber(Context().inputTickMultiplierRange)
      debug(s"MUTATION: mutateInputTickMultiplier for ${net.id} from ${net.inputTickMultiplier} to $newInputTickMultiplier")
      net.inputTickMultiplier = newInputTickMultiplier
  })

  private def invertSynapseWeight(weight: SynapseTrait) = weight match {
    case Hush() => SynapseWeight(1.0)
    case SynapseWeight(1.0) => Hush()
    case SynapseWeight(weight) => SynapseWeight(Context().weightRange.to - weight + Context().weightRange.from)
  }

  add("invertSynapse", (net: NetGenome) => chooseSynapse(net) match {
    case Some((s,n)) =>
      val newWeight = invertSynapseWeight(s.weight)
      debug(s"MUTATION: ... invert a synapse connecting from ${n.id} to ${s.neuronId}, from ${s.weight} to $newWeight")
      s.weight = newWeight
    case None =>
  })

  add("setWeightToHush", (net: NetGenome) => chooseSynapse(net) match {
    case Some((s,n)) =>
      debug(s"MUTATION: ... setWeightToHush in a synapse connecting from ${n.id} to ${s.neuronId}, from ${s.weight}")
      s.weight = Hush()
    case None =>
  })

  add("setWeightToFull", (net: NetGenome) => chooseSynapse(net) match {
    case Some((s,n)) =>
      debug(s"MUTATION: ... setWeightToFull in a synapse connecting from ${n.id} to ${s.neuronId}, from ${s.weight}")
      s.weight = SynapseWeight(1.0)
    case None =>
  })

  add("mutateWeight", (net: NetGenome) => chooseSynapse(net) match {
    case Some((s,n)) =>
      val newWeight = s.weight match {
        case SynapseWeight(avoidWeight) =>
          SynapseWeight(Context().weightRange.choose(RandomNumber(), avoidWeight))
        case Hush() =>
          SynapseWeight(RandomNumber(Context().weightRange))
      }
      debug(s"MUTATION: ... mutateWeight in a synapse connecting from ${n.id} to ${s.neuronId}, from ${s.weight} to $newWeight, range is: ${Context().weightRange}")
      s.weight = newWeight
    case None =>
  })

  add("invertNeuron", (net: NetGenome) => chooseNeuron(net) match {
    case Some(n) =>
      debug(s"MUTATION: invert for ${n.id}")
      n.synapses.foreach(s => s.weight = invertSynapseWeight(s.weight))
    case None =>
  })

  add("mutateThreshold", (net: NetGenome) => chooseNeuron(net) match {
    case Some(n) =>
      val newThreshold = RandomNumber(Context().thresholdRange)
      debug(s"MUTATION: mutateThreshold for ${net.id} from ${n.threshold} to $newThreshold")
      n.threshold = newThreshold
    case None =>
  })

  add("mutateSlope", (net: NetGenome) => chooseNeuron(net) match {
    case Some(n) =>
      val newSlope = RandomNumber(Context().slopeRange)
      debug(s"MUTATION: mutateSlope for ${net.id} from ${n.slope} to $newSlope")
      n.slope = newSlope
    case None =>
  })

  add("mutateHushValue", (net: NetGenome) => chooseNeuron(net) match {
    case Some(n) =>
      val newHushValue = HushValue(RandomNumber(Context().hushRange))
      debug(s"MUTATION: mutateHushValue for ${n.id} from ${n.hushValue} to $newHushValue")
      n.hushValue = newHushValue
    case None =>
  })

  add("mutateTickTimeMultiplier", (net: NetGenome) => chooseNeuron(net) match {
    case Some(n) =>
      val newTickTimeMultiplier = RandomNumber(Context().tickTimeMultiplierRange)
      debug(s"MUTATION: mutateHushValue for ${n.id} from ${n.tickTimeMultiplier} to $newTickTimeMultiplier")
      n.tickTimeMultiplier = newTickTimeMultiplier
    case None =>
  })

  add("setDontForget", (net: NetGenome) => chooseNeuron(net) match {
    case Some(n) =>
      debug(s"MUTATION: setDontForget for ${n.id} from ${n.forgetting} to ${DontForget()}")
      n.forgetting = DontForget()
    case None =>
  })

  add("mutateForgetValue", (net: NetGenome) => chooseNeuron(net) match {
    case Some(n) =>
      val newForgetting = ForgetValue(RandomNumber(Context().forgettingRange))
      debug(s"MUTATION: mutateForgetValue for ${n.id} from ${n.forgetting} to $newForgetting")
      n.forgetting = newForgetting
    case None =>
  })

  add("setForgetAll", (net: NetGenome) => chooseNeuron(net) match {
    case Some(n) =>
      debug(s"MUTATION: setDontForget for ${n.id} from ${n.forgetting} to ${ForgetAll()}")
      n.forgetting = ForgetAll()
    case None =>
  })

}
