package anna.async

import anna.Context
import anna.async.Messages._
import anna.async.NeuronType._
import anna.data.{DontForget, ForgetTrait, Hush, HushValue, NeuronData, SynapseData, SynapseTrait, SynapseWeight}
import anna.utils.Utils.{assert, await}

import scala.collection.mutable

class NetBuilder {
  var defThreshold = Context.threshold
  var defSlope = Context.slope
  var defHushValue = Context.hushValue
  var defForgetting: ForgetTrait = Context.forgetting
  val defTickTimeMultiplier: Double = 1.0

  var defWeight = Context.weight

  var defPrefix = "mi"
  var netName = "net"

  var inputTickMultiplier = 1.0

  private val neurons = mutable.Map[String,NeuronData]()
  private val synapses = mutable.Map[String,mutable.ListBuffer[SynapseData]]()
  private val ins = mutable.Set[String]()

  private var currentNeuronId:Option[String] = None

  def inputSet = ins.toSet

  def generateId() = defPrefix + nextIndex()
  
  def addSynapse(fromId: String, toId: String, weight: SynapseTrait) = {
    synapses.getOrElseUpdate(fromId, mutable.ListBuffer[SynapseData]()) += SynapseData(toId, weight)
    this
  }
    
  def contains(id: String) = neurons.contains(id)
  
  def use(id: String) = {
    assert(contains(id),s"There is no neuron with id $id")
    currentNeuronId = Some(id)
    this
  }
  
  def hush(id: String) = connect(id, Hush)

  def connect(id: String, weight: Double =defWeight):NetBuilder = connect(id, SynapseWeight(weight))
   
  def current = currentNeuronId match {
    case Some(id) => neurons(id)
    case None => throw new IllegalArgumentException("There is no current neuron id set")
  }
  
  def chain(id: String, weight: Double, threshold: Double, slope: Double,
            hushValue: HushValue, forgetting: ForgetTrait, tickTimeMultiplier: Double) = {
    val n1 = current
    addStandard(id, threshold, slope, hushValue, forgetting, tickTimeMultiplier)
    addSynapse(n1.id, id, SynapseWeight(weight))
    this
  }
  
  def chainDummy(id: String, weight: Double, hushValue: HushValue =defHushValue) = {
    val n1 = current
    addDummy(id)
    addSynapse(n1.id, id, SynapseWeight(weight))
    this
  }
  
  def chainHushNeuron(id: String) = {
    val n1 = current
    addHushNeuron(id)
    addSynapse(n1.id, id, Hush)
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
                forgetting: ForgetTrait = DontForget,
                tickTimeMultiplier: Double = defTickTimeMultiplier):NetBuilder =
    addStandard(id, threshold, slope, hushValue, forgetting, tickTimeMultiplier)

  
  def addMiddle():NetBuilder = addMiddle(generateId())

  def addHushNeuron(id: String) = {
    throwIfAlreadyExists(id)
    add(newNeuron(HUSH, id))
    this
  }

  def addDummy(id: String, tickTimeMultiplier: Double = defTickTimeMultiplier) = {
    throwIfAlreadyExists(id)
    add(newNeuron(neuronType=DUMMY, id=id, tickTimeMultiplier=tickTimeMultiplier))
    this
  }

  def addStandard(id: String,
                  threshold: Double,
                  slope:Double,
                  hushValue: HushValue,
                  forgetting: ForgetTrait,
                  tickTimeMultiplier: Double) = {
    throwIfAlreadyExists(id)
    add(newNeuron(STANDARD, id, threshold, slope, hushValue, forgetting, tickTimeMultiplier))
    this
  }

  def build() = {
    val net = NetRef(netName)
    val nRefs = neurons.values.par.map(
      nd => nd.id -> createNeuronInNet(net, nd.withoutSynapses)
    ).toMap

    nRefs.values.par.foreach(
      nRef => nRef.setSynapses(synapses.getOrElse(nRef.id, Nil).map(sd => Synapse(nRefs(sd.neuronId), sd.weight)))
    )
    
    net.setInputs(ins.toSeq)
    val sb = StringBuilder.newBuilder
    ins.foreach( sb.append(_).append(',') )
    sb.clear()
    net
  }
  
  def build(netInputName: String, netOutputName: String):(NetInput,NetRef) = {
    val net = build()
    val in = NetInput(netInputName, net, inputTickMultiplier)
    (in, net)
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
  
  private def newNeuron(neuronType: NeuronType.Value, id: String, 
      threshold: Double =defThreshold, slope: Double =defSlope, hushValue: HushValue =defHushValue,
      forgetting: ForgetTrait =defForgetting, tickTimeMultiplier: Double = defTickTimeMultiplier) =
    NeuronData(id, threshold, slope, hushValue, forgetting, Nil, tickTimeMultiplier, neuronType)

  private def connect(id: String, weight: SynapseTrait) = {
    addSynapse(current.id, id, weight)
    this
  }

  private def add(n: NeuronData){
    neurons.put(n.id, n)
    currentNeuronId = Some(n.id)
  }

  private def throwIfAlreadyExists(id: String) = assert(!contains(id), s"There is already a neuron with id $id")
}

object NetBuilder {
  def apply() = new NetBuilder()
}