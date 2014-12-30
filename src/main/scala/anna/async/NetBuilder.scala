package anna.async

import anna.async.Context.{INPUT_LAYER_NAME, MIDDLE_LAYER_NAME}
import anna.async.Messages._
import anna.async.NeuronType._
import anna.async.logger.LOG._
import anna.data.{DontForget, ForgetAll, ForgetTrait, Hush, HushValue, NeuronData, SynapseData, SynapseTrait, SynapseWeight}
import anna.utils.Utils.await

import scala.collection.mutable

class NetBuilder {
  var defSlope = Context.slope
  var defThreshold = Context.threshold
  var defWeight = Context.weight
  var defHushValue = Context.hushValue
  var defForgetting: ForgetTrait = Context.forgetting
  val defTickTime: Long = Context.tickTime

  var defInputName = INPUT_LAYER_NAME
  var defMiddleName = MIDDLE_LAYER_NAME
  var netName = "net"
  var inputNeuronType = NeuronType.DUMMY
  var middleNeuronType = NeuronType.STANDARD
  var outputNeuronType = NeuronType.DUMMY
  var inputTickMultiplicity = 1
  
  var throwOnError = true
  
  private val neurons = mutable.Map[String,NeuronData]()
  private val synapses = mutable.Map[String,mutable.ListBuffer[SynapseData]]()
  
  private val ins = mutable.Set[String]()
  private val mids = mutable.Set[String]()
  
  private var currentNeuronId:Option[String] = None
  
  def generateId(layer: String) = {
    val prefix = layer match {
      case INPUT_LAYER_NAME => defInputName
      case MIDDLE_LAYER_NAME => defMiddleName
    }
    
    prefix + nextIndex()
  }
  
  def addSynapse(fromId: String, toId: String, weight: SynapseTrait) =
    synapses.getOrElseUpdate(fromId, mutable.ListBuffer[SynapseData]()) += SynapseData(toId, weight)
    
  def contains(id: String) = neurons.contains(id)
  
  def use(id: String) = if(contains(id)){
    debug(this, s"using id")
    currentNeuronId = Some(id)
    this
  } else throw new IllegalArgumentException(s"There is no neuron with id $id")
  
  def hush(id: String):NetBuilder = connect(id, Hush)

  def connect(id: String, weight: Double =defWeight):NetBuilder = connect(id, SynapseWeight(weight))
   
  def current = currentNeuronId match {
    case Some(id) => neurons(id)
    case None => throw new IllegalArgumentException("There is no current neuron id set")
  }
  
  def chain(id: String, layerName: String, weight: Double, threshold: Double, 
            hushValue: HushValue, forgetting: ForgetTrait, slope: Double):NetBuilder = {
    val n1 = current
    debug(this, s"chaining from ${n1.id} to $id with treshold $threshold and slope $slope")
    
    add(id, layerName, threshold, hushValue, forgetting, slope)
    
    addSynapse(n1.id, id, SynapseWeight(weight))
    this
  }
  
  def chainDummy(id: String, weight: Double, hushValue: HushValue =defHushValue) = {
    val n1 = current
    debug(this, s"chaining a dummy from ${n1.id} to $id")
    
    addDummy(id)
    
    addSynapse(n1.id, id, SynapseWeight(weight))
    mids += id
    
    this
  }
  
  def chainHushNeuron(id: String) = {
    val n1 = current
    debug(this, s"chaining a hush neuron from ${n1.id} to $id")
    
    addHushNeuron(id)
    
    addSynapse(n1.id, id, Hush)
    mids += id
    
    this
  }
  
  def addInput(id: String, threshold: Double =0.0, hushValue: HushValue =defHushValue, slope:Double = defSlope):NetBuilder = 
    add(id, INPUT_LAYER_NAME, threshold, hushValue, ForgetAll, slope)
  
  def addMiddle(id: String, threshold: Double =defThreshold, hushValue: HushValue =defHushValue, 
                forgetting: ForgetTrait = DontForget, slope: Double = defSlope):NetBuilder = 
    add(id, MIDDLE_LAYER_NAME, threshold, hushValue, forgetting, slope)
  
  def addMiddle():NetBuilder = addMiddle(generateId(MIDDLE_LAYER_NAME))

  def addHushNeuron(id: String):NetBuilder = {
    if(contains(id)){
      if(!throwOnError) get(id) else throw new IllegalArgumentException(s"There is already a neuron with id $id")
    } else {
      add(newNeuron(HUSH, id))
    }
    this
  }
  
  def addHushNeuron():NetBuilder = addHushNeuron(generateId(MIDDLE_LAYER_NAME))
  
  def addDummy(id: String):NetBuilder = {
    if(contains(id)){
      if(!throwOnError) get(id) else throw new IllegalArgumentException(s"There is already a neuron with id $id")
    } else {
      add(newNeuron(DUMMY, id))
    }
    this
  }
  
  def addDummy():NetBuilder = addDummy(generateId(MIDDLE_LAYER_NAME))
  
  def build:NetRef = {
    debug(this, s"build()")
    
    val net = NetRef(netName)
    val nRefs = neurons.values.map{ nd => nd.id -> createNeuronInNet(net, nd) }.toMap
    nRefs.values.foreach(
      nRef => nRef.setSynapses(synapses.getOrElse(nRef.id, Nil).map(sd => Synapse(nRefs(sd.neuronId), sd.weight)))
    )
    
    net.setInputLayer(ins.toSeq)
    val sb = StringBuilder.newBuilder
    ins.foreach( sb.append(_).append(','))
    debug(this, s"input layer set request sent: ${sb.toString}")
    sb.clear()
    net
  }
  
  def build(netInputName: String, netOutputName: String):(NetInput,NetRef) = {
    debug(this, s"build($netInputName,$netOutputName)")
    val net = build
    val in = NetInput(netInputName, net, inputTickMultiplicity)
    debug(this, "net input built")
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
  
  private def getLayer(name: String) = name match {
    case INPUT_LAYER_NAME => (ins, inputNeuronType)
    case MIDDLE_LAYER_NAME => (mids, middleNeuronType)
  }
  
  private def newNeuron(neuronType: NeuronType.Value, id: String, 
      threshold: Double =defThreshold, slope: Double =defSlope, hushValue: HushValue =defHushValue,
      forgetting: ForgetTrait =defForgetting, tickTime: Long = defTickTime) =
    NeuronData(id, threshold, slope, hushValue, forgetting, Nil, tickTime, neuronType)

  private def get(id: String) = neurons(id)  
  
  private def connect(id: String, weight: SynapseTrait):NetBuilder = {
    addSynapse(current.id, id, weight)
    this
  }

  private def add(n: NeuronData) = {
    neurons.put(n.id, n)
    currentNeuronId = Some(n.id)
    n
  }

  private def add(id: String, layerName: String, treshold: Double, hushValue: HushValue, forgetting: ForgetTrait, slope:Double):NetBuilder = {
    debug(this, s"adding id with treshold $treshold, slope $slope, hush value $hushValue and forgetting $forgetting")
    val (layer, neuronType) = getLayer(layerName)
    val n = if(contains(id)){
      if(!throwOnError) get(id) else throw new IllegalArgumentException(s"There is already a neuron with id $id")
    } else {
      add(newNeuron(neuronType, id, treshold, slope, hushValue, forgetting)) 
    }
    layer += n.id
            
    this    
  }
}

object NetBuilder {
  def apply() = new NetBuilder()
}