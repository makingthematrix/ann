package anna.async

import anna.Context
import anna.data.SynapseData.fromDouble
import anna.data._
import anna.logger.LOG
import anna.logger.LOG._
import anna.utils.Utils.assert

import scala.collection.mutable

/**
 * Typed-Actor version of [[NetBuilder]].
 *
 * The accumulation DSL (neurons, synapses, inputs, defaults) is identical to
 * the classic builder — it is plain data, no actors involved. Only [[build]]
 * differs: it spawns a [[NetRefTyped]] and wires it up with [[NeuronRefTyped]] /
 * [[SynapseTyped]] instead of the classic [[NetRef]] / [[Synapse]], and returns
 * a [[NetWrapperTyped]].
 *
 * The `Double`-based convenience overloads and the `anna.blocks` DSL live in
 * `NetBuilderOps` and are not ported here; use the `SynapseTrait`-based methods
 * (`chain`, `connect`, `addStandard`, ...) directly.
 */
class NetBuilderTyped {
  var netId: String = "net"

  var defThreshold: Double = Context().threshold
  var defSilenceIterations: Int = Context().silenceIterations
  var defWeight: SynapseTrait = Context().weight

  private val neurons = mutable.Map[String, NeuronData]()
  private val synapses = mutable.Map[String, mutable.ListBuffer[SynapseData]]()
  private val ins = mutable.Set[String]()
  private val startSilent = mutable.Set[String]()

  private var currentNeuronId: Option[String] = None

  def inputSet: Set[String] = ins.toSet

  def generateId(): String = netId + "_" + nextIndex()

  private def addSynapse(fromId: String, toId: String, weight: SynapseTrait) =
    synapses.getOrElseUpdate(fromId, mutable.ListBuffer[SynapseData]()) += SynapseData(toId, weight)

  def contains(id: String): Boolean = neurons.contains(id)

  def use(id: String): NetBuilderTyped = {
    assert(contains(id), s"There is no neuron with id $id")
    currentNeuronId = Some(id)
    this
  }

  def silence(id: String): NetBuilderTyped = connect(id, Silence())
  def wake(id: String): NetBuilderTyped = connect(id, Wake())

  def connect(id: String, weight: SynapseTrait): NetBuilderTyped = {
    assert(contains(id), s"There is no neuron with id $id")
    addSynapse(current.id, id, weight)
    this
  }

  def current: NeuronData = currentNeuronId match {
    case Some(id) => neurons(id)
    case None => throw new IllegalArgumentException("There is no current neuron id set")
  }

  def isCurrent: Boolean = currentNeuronId.isDefined

  def chain(id: String, weight: SynapseTrait, threshold: Double, silenceIterations: Int): NetBuilderTyped = {
    val n1 = current
    addStandard(id, threshold, silenceIterations)
    addSynapse(n1.id, id, weight)
    this
  }

  def chainDummy(id: String, weight: Double, hushValue: Int = defSilenceIterations): NetBuilderTyped = {
    val n1 = current
    addDummy(id)
    addSynapse(n1.id, id, weight)
    this
  }

  def chainSilencingNeuron(id: String): NetBuilderTyped = {
    val n1 = current
    addSilencingNeuron(id)
    addSynapse(n1.id, id, Silence())
    this
  }

  def addInput(id: String): NetBuilderTyped = {
    addDummy(id)
    ins += id
    this
  }

  def initSilent(id: String): NetBuilderTyped = {
    startSilent += id
    this
  }

  def initSilent(): NetBuilderTyped = initSilent(current.id)

  def addMiddle(id: String,
                threshold: Double = defThreshold,
                silenceIterations: Int = defSilenceIterations): NetBuilderTyped =
    addStandard(id, threshold, silenceIterations)

  def addMiddle(): NetBuilderTyped = addMiddle(generateId())

  def addSilencingNeuron(id: String): NetBuilderTyped = {
    throwIfAlreadyExists(id)
    add(newNeuron(NeuronTypeSilencing(), id, 0.0))
    this
  }

  def addDummy(id: String): NetBuilderTyped = {
    throwIfAlreadyExists(id)
    add(newNeuron(neuronType = NeuronTypeDummy(), id = id, threshold = 0.0))
    this
  }

  def addStandard(id: String,
                  threshold: Double,
                  silenceIterations: Int): NetBuilderTyped = {
    debug(s"new neuron: $id")
    throwIfAlreadyExists(id)
    add(newNeuron(NeuronTypeStandard(), id, threshold, silenceIterations))
    this
  }

  def build(netName: String = netId): NetWrapperTyped = {
    debug(this, s"build $netName")
    val net = NetRefTyped(netName)

    debug(this, "building neurons")
    val nRefs = neurons.values.map(nd => {
      debug(this, s"creating $nd")
      nd.id -> createNeuronInNet(net, nd.withoutSynapses)
    }).toMap

    debug(this, "building synapses")
    nRefs.values.foreach(nRef => {
      val nsyns: Seq[SynapseTyped] =
        synapses.getOrElse(nRef.id, Nil).map(sd => SynapseTyped(nRefs(sd.neuronId), sd.weight)).toSeq
      debug(this, s"building synapses for ${nRef.id}: ${nsyns.size}")
      nRef.setSynapses(nsyns)
    })

    startSilent.map(nRefs(_)).foreach(_.requestSilence())

    debug(this, s"setting inputs: $ins")
    net.setInputs(ins.toList.sorted)
    debug(this, "done")
    NetWrapperTyped(net)
  }

  def data: NetData = NetData(
    netId,
    neurons.values.map(n =>
      n.withSynapses(synapses.getOrElse(n.id, Nil).toList)
    ).toList.sortBy(_.id),
    ins.toList.sorted,
    defThreshold,
    defSilenceIterations,
    defWeight
  )

  def set(data: NetData): NetBuilderTyped = {
    netId = data.id

    clear()

    data.neurons.foreach(n => {
      neurons += (n.id -> n)
      val buffer = mutable.ListBuffer[SynapseData]()
      n.synapses.foreach(buffer += _)
      synapses += (n.id -> buffer)
    })

    ins ++= data.inputs

    defThreshold = data.threshold
    defSilenceIterations = data.silenceIterations
    defWeight = data.weight

    this
  }

  def netId(id: String): NetBuilderTyped = {
    netId = id
    this
  }

  def clear(): NetBuilderTyped = {
    neurons.clear()
    synapses.clear()
    ins.clear()
    this
  }

  private def createNeuronInNet(net: NetRefTyped, data: NeuronData): NeuronRefTyped = data.neuronType match {
    case NeuronTypeStandard()  => net.createNeuron(data.id, data.threshold, data.silenceIterations)
    case NeuronTypeDummy()     => net.createDummy(data.id, data.silenceIterations)
    case NeuronTypeSilencing() => net.createSilencingNeuron(data.id)
  }

  private val nextIndex: () => Long = {
    var nextFreeIndex = 0L
    () => {
      val t = nextFreeIndex
      nextFreeIndex += 1L
      t
    }
  }

  private def newNeuron(neuronType: NeuronType,
                        id: String,
                        threshold: Double = defThreshold,
                        silenceIterations: Int = defSilenceIterations): NeuronData =
    NeuronData(id, threshold, silenceIterations, Nil, neuronType)

  private def add(n: NeuronData): Unit = {
    neurons.put(n.id, n)
    currentNeuronId = Some(n.id)
  }

  private def throwIfAlreadyExists(id: String): Unit = assert(!contains(id), s"There is already a neuron with id $id")
}

object NetBuilderTyped {
  def apply(): NetBuilderTyped = new NetBuilderTyped()

  def apply(name: String): NetBuilderTyped = {
    val nb = new NetBuilderTyped()
    nb.netId(name)
    nb
  }

  def apply(data: NetData): NetBuilderTyped = {
    val builder = new NetBuilderTyped()
    builder.set(data)
  }
}
