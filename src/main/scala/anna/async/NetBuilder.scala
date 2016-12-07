package anna.async

import anna.Context
import anna.async.Messages._
import anna.data.SynapseData.fromDouble
import anna.data._
import anna.logger.LOG._
import anna.utils.Utils.{assert, await}
import akka.pattern.ask
import scala.collection.mutable

import anna.logger.LOG

class NetBuilder {
  var netId:String = "net"

  var defThreshold = Context().threshold
  var defSilenceIterations = Context().silenceIterations
  var defWeight:SynapseTrait = Context().weight

  private val neurons = mutable.Map[String,NeuronData]()
  private val synapses = mutable.Map[String,mutable.ListBuffer[SynapseData]]()
  private val ins = mutable.Set[String]()

  private var currentNeuronId:Option[String] = None

  def inputSet = ins.toSet

  def generateId() = netId + "_" + nextIndex()

  private def addSynapse(fromId: String, toId: String, weight: SynapseTrait) =
    synapses.getOrElseUpdate(fromId, mutable.ListBuffer[SynapseData]()) += SynapseData(toId, weight)

  def contains(id: String) = neurons.contains(id)

  def use(id: String) = {
    assert(contains(id),s"There is no neuron with id $id")
    currentNeuronId = Some(id)
    this
  }

  def silence(id: String) = connect(id, Silence())

  def connect(id: String, weight: SynapseTrait) = {
    assert(contains(id),s"There is no neuron with id $id")
    addSynapse(current.id, id, weight)
    this
  }

  def current = currentNeuronId match {
    case Some(id) => neurons(id)
    case None => throw new IllegalArgumentException("There is no current neuron id set")
  }

  def isCurrent = currentNeuronId != None

  def chain(id: String, weight: SynapseTrait, threshold: Double,
            silenceIterations: Int) = {
    val n1 = current
    addStandard(id, threshold, silenceIterations)
    addSynapse(n1.id, id, weight)
    this
  }

  def chainDummy(id: String, weight: Double, hushValue: Int =defSilenceIterations) = {
    val n1 = current
    addDummy(id)
    addSynapse(n1.id, id, weight)
    this
  }

  def chainSilencingNeuron(id: String) = {
    val n1 = current
    addSilencingNeuron(id)
    addSynapse(n1.id, id, Silence())
    this
  }

  def addInput(id: String):NetBuilder = {
    addDummy(id)
    ins += id
    this
  }

  def addMiddle(id: String,
                threshold: Double =defThreshold,
                silenceIterations: Int =defSilenceIterations):NetBuilder =
    addStandard(id, threshold, silenceIterations)

  def addMiddle():NetBuilder = addMiddle(generateId())

  def addSilencingNeuron(id: String) = {
    throwIfAlreadyExists(id)
    add(newNeuron(NeuronTypeSilencing(), id))
    this
  }

  def addDummy(id: String) = {
    throwIfAlreadyExists(id)
    add(newNeuron(neuronType=NeuronTypeDummy(),
                  id=id, threshold=0.0
    ))
    this
  }

  def addStandard(id: String,
                  threshold: Double,
                  silenceIterations: Int) = {
    LOG.info("new neuron: " + id)
    throwIfAlreadyExists(id)
    add(newNeuron(NeuronTypeStandard(), id, threshold, silenceIterations))
    this
  }

  def build(netName: String =netId) = {
    debug(this,s"build $netName")
    val net = NetRef(netName)

    debug(this, "building neurons")
    val nRefs = neurons.values.map( nd => {
      debug(this, s"creating ${nd.id}")
      nd.id -> createNeuronInNet(net, nd.withoutSynapses)
    }).toMap

    debug(this, "building synapses")
    nRefs.values.foreach(nRef => {
      val nsyns = synapses.getOrElse(nRef.id, Nil).map(sd => Synapse(nRefs(sd.neuronId), sd.weight))
      debug(this, s"building synapses for ${nRef.id}: ${nsyns.size}")
      nRef.setSynapses(nsyns)
    })

    val insSeq = ins.toSeq
    debug(this, s"setting inputs: $insSeq")
    net.setInputs(insSeq)
    debug(this, "done")
    NetWrapper(net)
  }

  def data = NetData(
    netId,
    neurons.values.map( n =>
      n.withSynapses(synapses.getOrElse(n.id, Nil).toList)
    ).toList.sortBy( _.id ),
    ins.toList.sorted,
    defThreshold,
    defSilenceIterations,
    defWeight
  )

  def set(data: NetData) = {
    netId = data.id

    clear()

    data.neurons.foreach( n => {
      neurons += (n.id -> n)
      val buffer = mutable.ListBuffer[SynapseData]()
      n.synapses.foreach( buffer += _ )
      synapses += ( n.id -> buffer )
    })

    ins ++= data.inputs

    defThreshold = data.threshold
    defSilenceIterations = data.silenceIterations
    defWeight = data.weight

    this
  }

  def netId(id: String):NetBuilder = {
    netId = id
    this
  }

  def clear() = {
    neurons.clear()
    synapses.clear()
    ins.clear()
    this
  }

  private def createNeuronInNet(net: NetRef, data: NeuronData) = await[NeuronRef](net, CreateNeuron(data))

  private val nextIndex = {
    var nextFreeIndex = 0L
    () => {
      val t = nextFreeIndex
      nextFreeIndex += 1L
      t
    }
  }

  private def newNeuron(neuronType: NeuronType, id: String,
      threshold: Double =defThreshold, silenceIterations: Int =defSilenceIterations) =
    NeuronData(id, threshold, silenceIterations, Nil, neuronType)

  private def add(n: NeuronData){
    neurons.put(n.id, n)
    currentNeuronId = Some(n.id)
  }

  private def throwIfAlreadyExists(id: String) = assert(!contains(id), s"There is already a neuron with id $id")
}

object NetBuilder {
  def apply():NetBuilder = new NetBuilder()

  def apply(name: String):NetBuilder = {
    val nb = new NetBuilder()
    nb.netId(name)
    nb
  }

  def apply(data: NetData):NetBuilder = {
    val builder = new NetBuilder()
    builder.set(data)
  }
}