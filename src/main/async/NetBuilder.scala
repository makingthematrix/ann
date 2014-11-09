package main.async

import scala.collection.mutable
import akka.actor.ActorSystem
import main.logger.LOG._
import scala.concurrent.Await
import akka.util.Timeout
import scala.concurrent.duration._
import Messages._
import main.utils.Utils.await
import Context.{ INPUT_LAYER_NAME, MIDDLE_LAYER_NAME, OUTPUT_LAYER_NAME }

object NeuronType extends Enumeration {
  type NeuronType = Value
  val STANDARD, DUMMY = Value
}

import NeuronType._

class NetBuilder {
  var defSlope = Context.slope
  var defThreshold = Context.threshold
  var defWeight = Context.weight
  var defHushValue = Context.hushValue
  var defForgetting:ForgettingTick = Context.forgetting
  var resolution = 1
  var defInputName = INPUT_LAYER_NAME
  var defMiddleName = MIDDLE_LAYER_NAME
  var defOutputName = OUTPUT_LAYER_NAME
  var defNetName = "net"
  var inputNeuronType = NeuronType.DUMMY
  var middleNeuronType = NeuronType.STANDARD
  var outputNeuronType = NeuronType.DUMMY
  
  var throwOnError = true
  
  protected val neurons = mutable.Map[String,NeuronRef]()
  protected lazy val net = NetRef(defNetName, defSlope, defThreshold, defWeight)
  
  protected val ins = mutable.Set[String]()
  protected val mids = mutable.Set[String]()
  protected val outs = mutable.Set[String]()
  
  protected var currentNeuronId:Option[String] = None
  protected var nextFreeId = 0L
  
  protected def nextId() = {
    val t = nextFreeId
    nextFreeId += 1L
    t
  }
  
  protected def generateName(layer: String) = {
    val prefix = layer match {
      case INPUT_LAYER_NAME => defInputName
      case MIDDLE_LAYER_NAME => defMiddleName
      case OUTPUT_LAYER_NAME => defOutputName
    }
    
    prefix + nextId()
  }
  
  private def getLayer(name: String) = name match {
    case INPUT_LAYER_NAME => (ins, inputNeuronType)
    case MIDDLE_LAYER_NAME => (mids, middleNeuronType)
    case OUTPUT_LAYER_NAME => (outs, outputNeuronType)
  }
  
  protected def newNeuron(neuronType: NeuronType.Value, id: String, 
      threshold: Double =defThreshold, slope: Double =defSlope, hushValue: Double =defHushValue,
      forgetting: ForgettingTick =defForgetting) = neuronType match {
    case STANDARD => await[NeuronRef](net, CreateNeuron(id, threshold, slope, hushValue, forgetting))
    case DUMMY => await[NeuronRef](net, CreateDummy(id, hushValue))
  }

  def get(name: String) = neurons(name)  
  def contains(name: String) = neurons.contains(name)
  
  def use(name: String) = {
    debug(this, s"using $name")
    currentNeuronId = Some(get(name).id)
    this
  }
  
  def hush(name: String):NetBuilder = connect(name, Hush)
  def connect(name: String, weight: Double =defWeight):NetBuilder = connect(name, SynapseWeight(weight))
  private def connect(name: String, weight: SynapseWeight):NetBuilder = {
    current.connect(get(name), weight)
    this
  }
  
  def current = currentNeuronId match {
    case Some(id) => neurons(id)
    case None => throw new IllegalArgumentException("There is no current neuron id set")
  }
  
  def currentName = currentNeuronId match {
    case Some(id) => neurons.contains(id) match {
      case true => id
      case false => throw new IllegalArgumentException(s"Unable to find the name of the current neuron, even though its id is $id")
    }
    case None => throw new IllegalArgumentException("There is no current neuron id set")
  }
  
  protected def add(n: NeuronRef) = {
    neurons.put(n.id, n)
    currentNeuronId = Some(n.id)
    n
  }

  private def add(name: String, layerName: String, treshold: Double, slope:Double, hushValue: Double, forgetting: ForgettingTick):NetBuilder = {
    debug(this, s"adding $name with treshold $treshold, slope $slope, hush value $hushValue and forgetting $forgetting")
    val (layer, neuronType) = getLayer(layerName)
    val n = if(contains(name)){
      if(!throwOnError) get(name) else throw new IllegalArgumentException(s"There is already a neuron with name $name")
    } else {
      add(newNeuron(neuronType, name, treshold, slope, hushValue, forgetting)) 
    }
    layer += n.id
            
    this    
  }
  
  private def chain(name: String, layerName: String, weight: Double, threshold: Double, 
                    slope: Double, hushValue: Double, forgetting: ForgettingTick):NetBuilder = {
    val n1 = current
    debug(this, s"chaining from ${n1.id} to $name with treshold $threshold and slope $slope")
    if(throwOnError && outs.contains(n1.id))
      throw new IllegalArgumentException("You can chain a new neuron only from input or middle neurons")
    add(name, layerName, threshold, slope, hushValue: Double, forgetting)
    val n = neurons(currentNeuronId.get)
    n1.connect(n, weight)
    this
  }
  
  def chainDummy(name: String, weight: Double, hushValue: Double =defHushValue):NetBuilder = {
    val n1 = current
    debug(this, s"chaining a dummy from ${n1.id} to $name")
    
    if(throwOnError && outs.contains(n1.id))
      throw new IllegalArgumentException("You can chain a dummy only from input or middle neurons")
    
    if(contains(name)){
      if(!throwOnError) get(name) else throw new IllegalArgumentException(s"There is already a neuron with name $name")
    } else {
      add(newNeuron(DUMMY, name))
    }
    
    val n = current
    n1.connect(n, weight)
    
    mids += n.id
    
    this
  }
  
  def addInput(name: String, threshold: Double =0.0, slope:Double = defSlope, hushValue: Double =defHushValue):NetBuilder = 
    add(name, INPUT_LAYER_NAME, threshold, slope, hushValue, ForgetAll)
  def addInput():NetBuilder = addInput(generateName(INPUT_LAYER_NAME))
  
  def addMiddle(name: String, treshold: Double =defThreshold, slope: Double = defSlope, 
                hushValue: Double =defHushValue, forgetting: ForgettingTick = DontForget):NetBuilder = 
    add(name, MIDDLE_LAYER_NAME, treshold, slope, hushValue, forgetting)
  def addMiddle():NetBuilder = addInput(generateName(MIDDLE_LAYER_NAME))

  def chainMiddle(name: String, weight: Double =defWeight, treshold: Double =defThreshold, slope: Double =defSlope, 
                  hushValue: Double =defHushValue, forgetting: ForgettingTick =DontForget):NetBuilder = 
    chain(name, defMiddleName, weight, treshold, slope, hushValue, forgetting)
  def chain(name: String, weight: Double =defWeight, treshold: Double =defThreshold, slope: Double =defSlope, 
            hushValue: Double =defHushValue, forgetting: ForgettingTick =DontForget):NetBuilder =
      chainMiddle(name, weight, treshold, slope, hushValue, forgetting)
  def chainMiddle():NetBuilder = chainMiddle(generateName(MIDDLE_LAYER_NAME))
  def chainMiddle(weight: Double):NetBuilder = chainMiddle(generateName(MIDDLE_LAYER_NAME), weight)
  def chainMiddle(weight: Double, treshold: Double):NetBuilder = 
    chainMiddle(generateName(MIDDLE_LAYER_NAME), weight, treshold)
  def chainMiddle(weight: Double, treshold: Double, slope: Double):NetBuilder = 
    chainMiddle(generateName(MIDDLE_LAYER_NAME), weight, treshold, slope)
    
  def addOutput(name: String, treshold: Double =defThreshold, hushValue: Double =defHushValue, forgetting: ForgettingTick =DontForget):NetBuilder = 
    add(name, OUTPUT_LAYER_NAME, treshold, defSlope, hushValue, forgetting)
  def addOutput():NetBuilder = addInput(generateName(OUTPUT_LAYER_NAME))
  def chainOutput(name: String, weight: Double =defWeight, treshold: Double =defThreshold, 
                  hushValue: Double =defHushValue, forgetting: ForgettingTick =DontForget):NetBuilder = 
    chain(name, OUTPUT_LAYER_NAME, weight, treshold, defSlope, hushValue, forgetting)
  def chainOutput():NetBuilder = chainOutput(generateName(OUTPUT_LAYER_NAME))
  def chainOutput(weight: Double):NetBuilder = chainOutput(generateName(OUTPUT_LAYER_NAME), weight)
  def chainOutput(weight: Double, treshold: Double):NetBuilder = 
    chainOutput(generateName(OUTPUT_LAYER_NAME), weight, treshold)

  def loop(name: String, w1: Double =defWeight, treshold: Double =defThreshold, w2: Double =defWeight, slope: Double =defSlope):NetBuilder = {
    val n1 = current
    if(throwOnError && !mids.contains(n1.id))
      throw new IllegalArgumentException("You can loop only in the middle layer")
    chainMiddle(name, w1, treshold, slope)
    current.connect(n1, w2)
    currentNeuronId = Some(n1.id)
    NetBuilder.this
  }
    
  def loop():NetBuilder = loop(generateName(MIDDLE_LAYER_NAME))
  def loop(w1: Double, treshold: Double, w2: Double):NetBuilder = loop(generateName(MIDDLE_LAYER_NAME), w1, treshold, w2)
  def loop(w1: Double, w2: Double):NetBuilder = loop(generateName(MIDDLE_LAYER_NAME), w1, defThreshold, w2)
  
  def oscillator(name: String) = loop(name, 1.0, 0.5, -1.0)
  def oscillator() = loop(1.0, 0.5, -1.0)
  
  def chainOscillator(name: String, weight: Double, treshold: Double) = chainMiddle(name, weight, treshold).oscillator(name+"_osc")
  def chainOscillator(name: String, weight: Double) = chainMiddle(name, weight).oscillator(name+"_osc")
  def chainOscillator(weight: Double) = chainMiddle(weight).oscillator()
  
  def build:NetRef = {
    debug(NetBuilder.this, s"build()")
    net.setInputLayer(ins.toSeq)
    val sb = StringBuilder.newBuilder
    ins.foreach( sb.append(_).append(','))
    debug(NetBuilder.this, s"input layer set request sent: ${sb.toString}")
    sb.clear()
    net.setOutputLayer(outs.toSeq)
    outs.foreach( sb.append(_).append(','))
    debug(NetBuilder.this, s"output layer set request sent: ${sb.toString}")
    net
  }
  
  def build(netInputName: String, netOutputName: String):(NetInput,NetRef,NetOutput) = {
    debug(NetBuilder.this, s"build($netInputName,$netOutputName)")
    build
    val in = NetInput(netInputName, net, resolution)
    debug(NetBuilder.this, "net input built")
    val out = NetOutput(netOutputName, net)
    debug(NetBuilder.this, "net output built")
    (in, net, out)
  }
  
  def self(weight: Double =defWeight):NetBuilder = {
    current.connect(current, weight)
    this
  }
  
  def setForgetting(forgetting: ForgettingTick):NetBuilder = {
    current.setForgetting(forgetting)
    this
  }
  def setForgetting(forgetting: Double):NetBuilder = setForgetting(ForgetValue(forgetting))
}

object NetBuilder {
  def apply():NetBuilder = {
    debug("a new akka net builder created")
    new NetBuilder()
  }
}