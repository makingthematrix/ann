package main.async

import scala.collection.mutable
import akka.actor.ActorSystem
import main.async.logger.LOG._
import scala.concurrent.Await
import akka.util.Timeout
import scala.concurrent.duration._
import Messages._
import main.utils.Utils.await
import Context.{ INPUT_LAYER_NAME, MIDDLE_LAYER_NAME }
import main.data.SynapseTrait
import main.data.Hush
import main.data.SynapseWeight
import NeuronType._
import main.data.ForgetTrait
import main.data.HushValue
import main.data.ForgetAll
import main.data.DontForget
import main.data.SynapseData
import main.data.NeuronData


class NetBuilder {
  var defSlope = Context.slope
  var defThreshold = Context.threshold
  var defWeight = Context.weight
  var defHushValue = Context.hushValue
  var defForgetting: ForgetTrait = Context.forgetting
  var defInputName = INPUT_LAYER_NAME
  var defMiddleName = MIDDLE_LAYER_NAME
  var defNetName = "net"
  var inputNeuronType = NeuronType.DUMMY
  var middleNeuronType = NeuronType.STANDARD
  var outputNeuronType = NeuronType.DUMMY
  var inputTickMultiplicity = 1
  
  var throwOnError = true
  
  private val neurons = mutable.Map[String,NeuronData]()
  private val synapses = mutable.Map[String,mutable.ListBuffer[SynapseData]]()
  
  protected val ins = mutable.Set[String]()
  protected val mids = mutable.Set[String]()
  
  protected var currentNeuronId:Option[String] = None
  protected var nextFreeId = 0L
  
  protected def nextId() = {
    val t = nextFreeId
    nextFreeId += 1L
    t
  }
  
  protected def generateId(layer: String) = {
    val prefix = layer match {
      case INPUT_LAYER_NAME => defInputName
      case MIDDLE_LAYER_NAME => defMiddleName
    }
    
    prefix + nextId()
  }
  
  private def getLayer(name: String) = name match {
    case INPUT_LAYER_NAME => (ins, inputNeuronType)
    case MIDDLE_LAYER_NAME => (mids, middleNeuronType)
  }
  
  private def addSynapse(fromId: String, toId: String, weight: SynapseTrait) =
    synapses.getOrElseUpdate(fromId, mutable.ListBuffer[SynapseData]()) += SynapseData(toId, weight)
  
  /*private def newNeuron(neuronType: NeuronType.Value, id: String, 
      threshold: Double =defThreshold, slope: Double =defSlope, hushValue: HushValue =defHushValue,
      forgetting: ForgetTrait =defForgetting) = neuronType match {
    case STANDARD => await[NeuronRef](net, CreateNeuron(id, threshold, slope, hushValue, forgetting))
    case DUMMY => await[NeuronRef](net, CreateDummy(id, hushValue))
    case HUSH => await[NeuronRef](net, CreateHushNeuron(id))
  }*/
    
  private def newNeuron(neuronType: NeuronType.Value, id: String, 
      threshold: Double =defThreshold, slope: Double =defSlope, hushValue: HushValue =defHushValue,
      forgetting: ForgetTrait =defForgetting) 
    = NeuronData(id, threshold, slope, hushValue, forgetting, Nil, neuronType)

  private def get(id: String) = neurons(id)  
  def contains(id: String) = neurons.contains(id)
  
  def use(id: String) = if(contains(id)){
    debug(this, s"using id")
    currentNeuronId = Some(id)
    this
  } else throw new IllegalArgumentException(s"There is no neuron with id $id")
  
  def hush(id: String):NetBuilder = connect(id, Hush)
  def connect(id: String, weight: Double =defWeight):NetBuilder = connect(id, SynapseWeight(weight))
  private def connect(id: String, weight: SynapseTrait):NetBuilder = {
    addSynapse(current.id, id, weight)
    this
  }
  
  def current = currentNeuronId match {
    case Some(id) => neurons(id)
    case None => throw new IllegalArgumentException("There is no current neuron id set")
  }
  
  protected def add(n: NeuronData) = {
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
  
  private def chain(id: String, layerName: String, weight: Double, threshold: Double, 
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
  
  def chainMiddle(id: String, weight: Double =defWeight, threshold: Double =defThreshold, hushValue: HushValue =defHushValue, 
                  forgetting: ForgetTrait =DontForget, slope: Double =defSlope):NetBuilder = 
    chain(id, defMiddleName, weight, threshold, hushValue, forgetting, slope)
  def chain(id: String, weight: Double =defWeight, threshold: Double =defThreshold, hushValue: HushValue =defHushValue, 
            forgetting: ForgetTrait =DontForget, slope: Double =defSlope):NetBuilder =
      chainMiddle(id, weight, threshold, hushValue, forgetting, slope)
  def chainMiddle():NetBuilder = chainMiddle(generateId(MIDDLE_LAYER_NAME))
  def chainMiddle(weight: Double):NetBuilder = chainMiddle(generateId(MIDDLE_LAYER_NAME), weight)
  def chainMiddle(weight: Double, treshold: Double):NetBuilder = 
    chainMiddle(generateId(MIDDLE_LAYER_NAME), weight, treshold)
  def chainMiddle(weight: Double, treshold: Double, slope: Double):NetBuilder = 
    chainMiddle(generateId(MIDDLE_LAYER_NAME), weight, treshold, defHushValue, defForgetting, slope)
    
  def loop(id: String, w1: Double =defWeight, treshold: Double =defThreshold, w2: Double =defWeight, slope: Double =defSlope):NetBuilder = {
    val n1 = current
    if(throwOnError && !mids.contains(n1.id))
      throw new IllegalArgumentException("You can loop only in the middle layer")
    
    chainMiddle(id, w1, treshold, defHushValue, defForgetting, slope)
    
    addSynapse(id, n1.id, SynapseWeight(w2))
    currentNeuronId = Some(n1.id)
    NetBuilder.this
  }
    
  def loop():NetBuilder = loop(generateId(MIDDLE_LAYER_NAME))
  def loop(w1: Double, treshold: Double, w2: Double):NetBuilder = loop(generateId(MIDDLE_LAYER_NAME), w1, treshold, w2)
  def loop(w1: Double, w2: Double):NetBuilder = loop(generateId(MIDDLE_LAYER_NAME), w1, defThreshold, w2)
  
  def oscillator(name: String) = loop(name, 1.0, 0.5, -1.0)
  def oscillator() = loop(1.0, 0.5, -1.0)
  
  def chainOscillator(name: String, weight: Double, treshold: Double) = chainMiddle(name, weight, treshold).oscillator(name+"_osc")
  def chainOscillator(name: String, weight: Double) = chainMiddle(name, weight).oscillator(name+"_osc")
  def chainOscillator(weight: Double) = chainMiddle(weight).oscillator()
  
  def self(weight: Double =defWeight):NetBuilder = {
    addSynapse(current.id, current.id, SynapseWeight(weight))
    this
  }
  
  private def createNeuronInNet(net: NetRef, data: NeuronData) = data.neuronType match {
    case STANDARD => await[NeuronRef](net, CreateNeuron(data.id, data.threshold, data.slope, data.hushValue, data.forgetting))
    case DUMMY => await[NeuronRef](net, CreateDummy(data.id, data.hushValue))
    case HUSH => await[NeuronRef](net, CreateHushNeuron(data.id))
  }
  
  def build:NetRef = {
    debug(this, s"build()")
    
    val net = NetRef(defNetName)
    val nRefs = neurons.values.map{ nd => (nd.id -> createNeuronInNet(net, nd)) }.toMap
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
  
}

object NetBuilder {
  def apply():NetBuilder = {
    debug("a new akka net builder created")
    new NetBuilder()
  }
}