package main

import scala.collection.mutable

object NeuronType extends Enumeration {
  type NeuronType = Value
  val STANDARD, DUMMY, DELAY = Value
}

class NetBuilder {
  var defSlope = NetBuilder.SLOPE
  var defTreshold = NetBuilder.TRESHOLD
  var defWeight = NetBuilder.WEIGHT
  var defInputName = "in"
  var defMiddleName = "mid"
  var defOutputName = "out"
  var defForgetting = NetBuilder.FORGETTING

  var inputNeuronType = NeuronType.DUMMY
  var middleNeuronType = NeuronType.STANDARD
  var outputNeuronType = NeuronType.DUMMY
  var resolution = 1
  
  var throwOnError = true
  
  private val neurons = mutable.Map[String,Neuron]()
  private val ins = mutable.Set[String]()
  private val mids = mutable.Set[String]()
  private val outs = mutable.Set[String]()
  
  private var currentNeuronId:Option[String] = None
  private var nextFreeId = 0L
  
  private def nextId() = {
    val t = nextFreeId
    nextFreeId += 1L
    t
  }
  
  private def generateName(layer: String) = {
    val prefix = layer match {
      case NetBuilder.INPUT_LAYER => defInputName
      case NetBuilder.MIDDLE_LAYER => defMiddleName
      case NetBuilder.OUTPUT_LAYER => defOutputName
    }
    
    prefix + nextId()
  }

  private def newNeuron(neuronType: NeuronType.Value, id: String, treshold: Double =defTreshold, slope: Double =defSlope, forgetting: Double = defForgetting) = neuronType match {
    case NeuronType.DUMMY => DummyNeuron(id, treshold)
    case NeuronType.STANDARD => Neuron(id, treshold, slope, forgetting)
    case NeuronType.DELAY => DelayNeuron(id, treshold, slope, forgetting)
  }
  
  private def newInput(id: String, treshold: Double =defTreshold, slope: Double =defSlope, forgetting: Double = defForgetting) = {
    val n = newNeuron(inputNeuronType, id, treshold, slope)
    n.priority = ins.size
    n
  }
  
  private def newMiddle(id: String, treshold: Double =defTreshold, slope: Double =defSlope) = {
    val n = newNeuron(middleNeuronType, id, treshold, slope)
    n.priority = mids.size
    n
  }
  
  private def newOutput(id: String, treshold: Double =defTreshold, slope: Double =defSlope) = {
    val n = newNeuron(outputNeuronType, id, treshold, slope)
    n.priority = outs.size
    n
  }
  
  private def add(n: Neuron) = {
    neurons.put(n.id, n)
    currentNeuronId = Some(n.id)
    n
  }
  
  def isCurrentNeuronIdSet = currentNeuronId == None
  
  def clearCurrentNeuronId = currentNeuronId = None
  
  def get(name: String) = neurons(name)
  
  def contains(name: String) = neurons.contains(name)
  
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
  
  def names = neurons.keySet
  def size = neurons.size
  
  def inSize = ins.size
  def midSize = mids.size
  def outSize = outs.size
  
  def use(name: String) = {
    println(s"using $name")
    currentNeuronId = Some(get(name).id)
    this
  }
  
  def connect(name: String, weight: Double =defWeight):NetBuilder = {
    current.connect(get(name), weight)
    this
  }

  def addInput(name: String, treshold: Double =0.0):NetBuilder = {
    val n = if(contains(name)){
      if(!throwOnError) get(name) else throw new IllegalArgumentException(s"There is already a neuron with name $name")
    } else add(newInput(name, treshold)) 
            
    ins += n.id
    this
  }
  def addInput():NetBuilder = addInput(generateName(NetBuilder.INPUT_LAYER))
  
  def addMiddle(name: String, treshold: Double =defTreshold, slope: Double = defSlope):NetBuilder = {
    val n = if(contains(name)){
      if(!throwOnError) get(name) else throw new IllegalArgumentException(s"There is already a neuron with name $name")
    } else add(newMiddle(name, treshold, slope))
            
    mids += n.id
    this
  }
  def addMiddle():NetBuilder = addMiddle(generateName(NetBuilder.MIDDLE_LAYER))
  
  def addOutput(name: String, treshold: Double =0.0):NetBuilder = {
    val n = if(contains(name)){ 
      if(!throwOnError) get(name) else throw new IllegalArgumentException(s"There is already a neuron with name $name")
    } else add(newOutput(name, treshold))
            
    outs += n.id
    this
  }
  def addOutput():NetBuilder = addOutput(generateName(NetBuilder.OUTPUT_LAYER))
  
  def chainMiddle(name: String, weight: Double =defWeight, treshold: Double =defTreshold, slope: Double =defSlope):NetBuilder = {
    val n1 = current
    println(s"chaining from ${n1.id} to $name")
    if(throwOnError && outs.contains(n1.id))
      throw new IllegalArgumentException("You can chain a new neuron in the middle layer only from input or other middle neurons")
    addMiddle(name, treshold, slope)
    n1.connect(neurons(currentNeuronId.get), weight)
    this
  }
  def chainMiddle():NetBuilder = chainMiddle(generateName(NetBuilder.MIDDLE_LAYER))
  def chainMiddle(weight: Double):NetBuilder = chainMiddle(generateName(NetBuilder.MIDDLE_LAYER), weight)
  def chainMiddle(weight: Double, treshold: Double):NetBuilder = chainMiddle(generateName(NetBuilder.MIDDLE_LAYER), weight, treshold)
  def chainMiddle(weight: Double, treshold: Double, slope: Double):NetBuilder = 
    chainMiddle(generateName(NetBuilder.MIDDLE_LAYER), weight, treshold, slope)
  
  def chainOutput(name: String, weight: Double =defWeight, treshold: Double =0.0):NetBuilder = {
    val n1 = current
    if(throwOnError && outs.contains(n1.id))
      throw new IllegalArgumentException("You can chain a new neuron in the output layer only to input or middle neurons")
    addOutput(name, treshold)
    n1.connect(neurons(currentNeuronId.get), weight)
    this
  }
  def chainOutput():NetBuilder = chainOutput(generateName(NetBuilder.OUTPUT_LAYER))
  def chainOutput(weight: Double):NetBuilder = chainOutput(generateName(NetBuilder.OUTPUT_LAYER), weight)
  def chainOutput(weight: Double, treshold: Double):NetBuilder = chainOutput(generateName(NetBuilder.OUTPUT_LAYER), weight, treshold)
  
  def loop(name: String, w1: Double =defWeight, treshold: Double =defTreshold, w2: Double =defWeight, slope: Double =defSlope):NetBuilder = {
    val n1 = current
    if(throwOnError && !mids.contains(n1.id))
      throw new IllegalArgumentException("You can loop only in the middle layer")
    chainMiddle(name, w1, treshold, slope)
    current.connect(n1, w2)
    currentNeuronId = Some(n1.id)
    this
  }
  
  def loop():NetBuilder = loop(generateName(NetBuilder.MIDDLE_LAYER))
  def loop(w1: Double, treshold: Double, w2: Double):NetBuilder = loop(generateName(NetBuilder.MIDDLE_LAYER), w1, treshold, w2)
  def loop(w1: Double, w2: Double):NetBuilder = loop(generateName(NetBuilder.MIDDLE_LAYER), w1, defTreshold, w2)
  
  def oscillator(name: String) = loop(name, 1.0, 0.5, -1.0)
  def oscillator() = loop(1.0, 0.5, -1.0)
  
  def chainOscillator(name: String, weight: Double, treshold: Double) = chainMiddle(name, weight, treshold).oscillator(name+"_osc")
  def chainOscillator(name: String, weight: Double) = chainMiddle(name, weight).oscillator(name+"_osc")
  def chainOscillator(weight: Double) = chainMiddle(weight).oscillator()
  
  def setForgetting(forgetting: Double) = {
    current.forgetting = forgetting
    this
  }
  
  def setPriority(priority: Int) = {
    current.priority = priority
    this
  }
  def priority = current.priority
  
  def self(weight: Double =defWeight):NetBuilder = {
    current.connect(current, weight)
    this
  }
  
  def build:Net = {
    val net = Net(defSlope, defTreshold, defWeight)
    neurons.foreach(tuple => net.addNeuron(tuple._2))
    net.setInputLayer(ins.toSeq)
    net.setOutputLayer(outs.toSeq)
    net
  }
  
  def build(netInputName: String, netOutputName: String):(NetInput,Net,NetOutput) = {
    val net = build
    val in = NetInput(netInputName, net, resolution)
    val out = NetOutput(netOutputName, net)
    (in, net, out)
  }
}

object NetBuilder {
  val SLOPE = 20.0
  val TRESHOLD = 0.5
  val WEIGHT = 1.0
  val FORGETTING = 0.0
  val RESOLUTION = 1
  
  val INPUT_LAYER = "in"
  val MIDDLE_LAYER = "mid"
  val OUTPUT_LAYER = "out"
  
  def apply():NetBuilder = new NetBuilder()
  def apply(middleNeuronType: NeuronType.Value, slope: Double = SLOPE, forgetting: Double = FORGETTING, resolution: Int = RESOLUTION):NetBuilder = {
    val builder = new NetBuilder
    builder.middleNeuronType = middleNeuronType
    builder.defSlope = slope
    builder.defForgetting = forgetting
    builder.resolution = resolution
    builder
  }
  
  //implicit def toNetBuilderOps(builder: NetBuilder) = new NetBuilderOps(builder) 
}