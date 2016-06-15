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
        val s = RandomNumber(n.synapses)
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
    if(forDeletion) neuronChooser.chooseForDeletion(net) else neuronChooser.chooseForMutation(net)
  private def chooseSynapse(net: NetGenome) = synapseChooser.choose(net)

  private val mutationsMap = mutable.Map[String, Mutation]()

  def get(name: String) = mutationsMap(name)

  def contains(name: String) = mutationsMap.contains(name)

  def names = mutationsMap.keys

  def add(name: String, mutation: Mutation):Unit = if(mutationsMap.contains(name)){
    throw new IllegalArgumentException(s"There is already a mutation with the name $name in the MutationsLibrary")
  } else mutationsMap += (name -> mutation)

  def mutate(genome: NetGenome, mutationName: String) = mutationsMap(mutationName)(genome)

  // ---

  add("addDelayGate", (net: NetGenome) => if(net.neurons.size >= 2 && net.mutableNeurons.size >= 1){
    // requirements: at least two neurons in the net and at least one of them full access mutable

    val block = DelayGate(RandomNumber(Context().fwdDelayRange))
    debug(s"MUTATION: addDelayGate to ${net.id} -> the delay is ${block.delay}")

    val inFrom = RandomNumber(net.neurons)
    val outTo = RandomNumber(net.mutableNeurons)
    val inHush = RandomNumber(net.neurons - inFrom)

    net.addData(block.data)

    inFrom.connect(net.find(block.inputId).get)
    net.find(block.outputId).get.connect(outTo)
    inHush.connect(net.find(block.hushId).get)
  })

  add("deleteDelayGate", (net: NetGenome) => {
    val blockNames = DelayGate.blocksInGenome(net)
    if(blockNames.nonEmpty) {
      val chosenBlockName = RandomNumber(blockNames)
      debug(s"MUTATION: deleteDelayGate with ${net.id} -> removing block $chosenBlockName")

      val blockNeuronIds = net.fullAccessNeurons.map(_.id).filter(_.contains(chosenBlockName))
      val inFromIds = net.findIdsConnectedTo(DelayGate.inputId(chosenBlockName)).filterNot(_.contains(chosenBlockName))
      val outToIds = (net.find(DelayGate.outputId(chosenBlockName)) match {
        case Some(n) => n.synapses.map(_.neuronId)
        case None => Nil
      }).toList.filterNot(_.contains(chosenBlockName))

      blockNeuronIds.foreach(net.deleteNeuron)
      blockNeuronIds.foreach(net.deleteSynapsesTo)
      inFromIds.zip(outToIds).foreach {
        case (inId, outId) => if (inId != outId && !net.isConnected(inId, outId)) net.connect(inId, outId)
      }
    }
  })

  add("modifyDelayGate", (net: NetGenome) => {

  })

  // ---

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

  // @todo: make it smarter - reconnect the synapses to one of neurons the deleted one sent signals to
  add("deleteNeuron", (net: NetGenome) => chooseNeuron(net, true) match {
    case Some(n) =>
      debug(s"MUTATION: deleteNeuron from ${net.id} -> the deleted neuron's id is ${n.id}")
      net.deleteNeuron(n.id)
      net.deleteSynapsesTo(n.id)
    case None =>
  })

  private def invertSynapseWeight(weight: SynapseTrait) = weight match {
    case Hush() => SynapseWeight(1.0)
    case SynapseWeight(1.0) => Hush()
    case SynapseWeight(weight) => SynapseWeight(Context().weightRange.to - weight + Context().weightRange.from)
  }

  add("addSynapse", (net: NetGenome) => chooseNeuron(net) match {
    case Some(n2) =>
      val validNeurons = net.neurons.filterNot(_.isConnectedTo(n2.id))
      if(validNeurons.nonEmpty) {
        val n1 = RandomNumber(validNeurons)
        val sg = n1.connect(n2)
        debug(s"MUTATION: ... add a synapse connecting ${n1.id} to ${n2.id}, with weight $sg")
      } else debug(s"MUTATION: ... failed to connect as all other valid neurons are already connected to ${n2.id} (and the author is too lazy to avoid such situation)")
    case None =>
  })

  add("deleteSynapse", (net: NetGenome) => {
    val validNeurons = net.neurons.filter(_.synapses.nonEmpty)
    if(validNeurons.nonEmpty){
        val n1 = RandomNumber(validNeurons)
        val n2Id = RandomNumber(n1.synapses.map(_.neuronId))
        n1.deleteSynapse(n2Id)
        debug(s"MUTATION: ... delete a synapse connecting ${n1.id} to ${n2Id}")
    } else debug(s"MUTATION: ... failed to delete a synapse - no synapses in the net")
  })

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

  add("mutateHushValue", (net: NetGenome) => chooseNeuron(net) match {
    case Some(n) =>
      val newHushValue = HushValue(RandomNumber(Context().hushRange))
      debug(s"MUTATION: mutateHushValue for ${n.id} from ${n.hushValue} to $newHushValue")
      n.hushValue = newHushValue
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
