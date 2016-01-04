package anna.async

import anna.Context
import anna.async.Messages._
import anna.data.SynapseData.fromDouble
import anna.data._
import anna.utils.Utils.{assert, await}
import scala.collection.mutable

class NetBuilder {
  var netId:String = "net"

  var defThreshold = Context().threshold
  var defSlope = Context().slope
  var defHushValue = Context().hushValue
  var defForgetting: ForgetTrait = Context().forgetting
  var defTickTimeMultiplier = 1.0
  var defWeight:SynapseTrait = Context().weight
  var inputTickMultiplier = 1.0
  var activationFunctionName = Context().activationFunctionName

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

  def hush(id: String) = connect(id, Hush())

  def connect(id: String, weight: SynapseTrait) = {
    assert(contains(id),s"There is no neuron with id $id")
    addSynapse(current.id, id, weight)
    this
  }

  def current = currentNeuronId match {
    case Some(id) => neurons(id)
    case None => throw new IllegalArgumentException("There is no current neuron id set")
  }

  def chain(id: String, weight: SynapseTrait, threshold: Double, slope: Double,
            hushValue: HushValue, forgetting: ForgetTrait, tickTimeMultiplier: Double) = {
    val n1 = current
    addStandard(id, threshold, slope, hushValue, forgetting, tickTimeMultiplier)
    addSynapse(n1.id, id, weight)
    this
  }

  def chainDummy(id: String, weight: Double, hushValue: HushValue =defHushValue) = {
    val n1 = current
    addDummy(id)
    addSynapse(n1.id, id, weight)
    this
  }

  def chainHushNeuron(id: String) = {
    val n1 = current
    addHushNeuron(id)
    addSynapse(n1.id, id, Hush())
    this
  }

  def addInput(id: String, tickTimeMultiplier: Double = defTickTimeMultiplier):NetBuilder = {
    addDummy(id, tickTimeMultiplier)
    ins += id
    this
  }

  def addMiddle(id: String,
                threshold: Double =defThreshold,
                slope: Double = defSlope,
                hushValue: HushValue =defHushValue,
                forgetting: ForgetTrait = DontForget(),
                tickTimeMultiplier: Double = defTickTimeMultiplier):NetBuilder =
    addStandard(id, threshold, slope, hushValue, forgetting, tickTimeMultiplier)


  def addMiddle():NetBuilder = addMiddle(generateId())

  def addHushNeuron(id: String) = {
    throwIfAlreadyExists(id)
    add(newNeuron(NeuronTypeHush(), id, activationFunctionName = ActivationFunction.UNUSED))
    this
  }

  def addDummy(id: String, tickTimeMultiplier: Double = defTickTimeMultiplier) = {
    throwIfAlreadyExists(id)
    add(newNeuron(neuronType=NeuronTypeDummy(),
                  id=id, threshold=0.0,
                  forgetting=ForgetAll(),
                  tickTimeMultiplier=tickTimeMultiplier,
                  activationFunctionName = ActivationFunction.UNUSED
    ))
    this
  }

  def addStandard(id: String,
                  threshold: Double,
                  slope:Double,
                  hushValue: HushValue,
                  forgetting: ForgetTrait,
                  tickTimeMultiplier: Double) = {
    throwIfAlreadyExists(id)
    add(newNeuron(NeuronTypeStandard(), id, threshold, slope, hushValue, forgetting, tickTimeMultiplier))
    this
  }

  def build(netName: String =netId) = {
    //debug(this,s"build $netName")
    val net = NetRef(netName)
    //debug(this, "building neurons")
    val nRefs = neurons.values.map(
      nd => nd.id -> createNeuronInNet(net, nd.withoutSynapses)
    ).toMap
    //debug(this, "building synapses")
    nRefs.values.foreach(nRef => {
      val nsyns = synapses.getOrElse(nRef.id, Nil).map(sd => Synapse(nRefs(sd.neuronId), sd.weight))
      //debug(this, s"building synapses for ${nRef.id}: ${nsyns.size}")
      nRef.setSynapses(nsyns)
    })
    //debug(this, "setting inputs")
    net.setInputs(ins.toSeq)
    //debug(this, "done")
    NetWrapper(net, inputTickMultiplier)
  }

  def data = NetData(
    netId,
    neurons.values.map( n => n.withSynapses(synapses.getOrElse(n.id, Nil).toList) ).toList.sortBy( _.id ),
    ins.toList.sorted,
    defThreshold, defSlope, defHushValue, defForgetting,
    defTickTimeMultiplier, defWeight, inputTickMultiplier,
    activationFunctionName
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
    defSlope = data.slope
    defHushValue = data.hushValue
    defForgetting = data.forgetting
    defTickTimeMultiplier = data.tickTimeMultiplier
    defWeight = data.weight
    inputTickMultiplier = data.inputTickMultiplier
    activationFunctionName = data.activationFunctionName

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
      threshold: Double =defThreshold, slope: Double =defSlope, hushValue: HushValue =defHushValue,
      forgetting: ForgetTrait =defForgetting, tickTimeMultiplier: Double = defTickTimeMultiplier,
      activationFunctionName: String = activationFunctionName) =
    NeuronData(
      id, threshold, slope, hushValue, forgetting, Nil, tickTimeMultiplier, neuronType, activationFunctionName
    )

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