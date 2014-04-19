package main

import scala.collection.mutable

class NetBuilder {
  var defSlope = NetBuilder.SLOPE
  var defTreshold = NetBuilder.TRESHOLD
  var defWeight = NetBuilder.WEIGHT
  var defInputName = "in"
  var defMiddleName = "mid"
  var defOutputName = "out"
  
  private val neurons = mutable.Map[Long,Neuron]()
  private val neuronNames = mutable.Map[String,Long]() 
  private val ins = mutable.Set[Long]()
  private val mids = mutable.Set[Long]()
  private val outs = mutable.Set[Long]()
  
  private var currentNeuronId:Option[Long] = None
  private var nextFreeId = 1L
  
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
    
    while(neuronNames.contains(prefix + nextFreeId)) nextId()
    prefix + nextFreeId
  }

  private def add(name: String, n: Neuron) = {
    neurons.put(n.id, n)
    neuronNames.put(name, n.id)
    currentNeuronId = Some(n.id)
    n
  }
  
  private def get(name: String) = neurons(neuronNames(name))
 
  def isCurrentNeuronIdSet = currentNeuronId == None
  
  def clearCurrentNeuronId = currentNeuronId = None
  
  def findByName(name: String) = neuronNames.get(name) match {
    case Some(id) => neurons(id)
    case None => throw new IllegalArgumentException(s"There is no neuron with the name alias $name")
  }
  
  def current = currentNeuronId match {
    case Some(id) => neurons(id)
    case None => throw new IllegalArgumentException("There is no current neuron id set")
  }
  
  def currentName = currentNeuronId match {
    case Some(id) => neuronNames.find(tuple => tuple._2 == id) match {
      case Some((name, id)) => Some(name)
      case None => throw new IllegalArgumentException(s"Unable to find the name of the current neuron, even though its id is $id")
    }
    case None => throw new IllegalArgumentException("There is no current neuron id set")
  }
  
  def ids = neurons.keySet
  def names = neuronNames.keySet
  def namesToIds = neuronNames.clone
  
  private def layerNames(layer: mutable.Set[Long]) = layer.map( id => neuronNames.find( tuple => tuple._2 == id) match {
    case Some((name, id)) => name
    case None => throw new IllegalArgumentException(s"Unable to find the name for the neuron with id $id")
  })
  
  def inputNames = layerNames(ins)
  def middleNames = layerNames(mids)
  def outputNames = layerNames(outs)
  
  def size = neurons.size
  
  def inSize = ins.size
  def midSize = mids.size
  def outSize = outs.size
  
  def use(name: String) = {
    currentNeuronId = Some(findByName(name).id)
    this
  }
  
  def connect(name: String, weight: Double =defWeight):NetBuilder = {
    current.connect(findByName(name), weight)
    this
  }

  def addInput(name: String, treshold: Double =0.0):NetBuilder = {
    println(s"adding input neuron with name $name and treshold $treshold")
    val n = add(name, DummyNeuron(-nextId(), treshold))
    ins += n.id
    this
  }
  def addInput():NetBuilder = addInput(generateName(NetBuilder.INPUT_LAYER))
  
  def addMiddle(name: String, treshold: Double =defTreshold, slope: Double = defSlope):NetBuilder = {
    println(s"adding middle neuron with name $name, treshold $treshold and slope $slope")
    val n = add(name, new Neuron(nextId(), treshold, slope))
    mids += n.id
    this
  }
  def addMiddle():NetBuilder = addMiddle(generateName(NetBuilder.MIDDLE_LAYER))
  
  def addOutput(name: String, treshold: Double =0.0):NetBuilder = {
    println(s"adding output neuron with name $name and treshold $treshold")
    val n = add(name, DummyNeuron(-nextId(), treshold))
    outs += n.id
    this
  }
  def addOutput():NetBuilder = addOutput(generateName(NetBuilder.OUTPUT_LAYER))
  
  def chainMiddle(name: String, weight: Double =defWeight, treshold: Double =defTreshold, slope: Double =defSlope):NetBuilder = {
    val n1 = current
    if(outs.contains(n1.id))
      throw new IllegalArgumentException("You can chain a new neuron in the middle layer only to input or other middle neurons")
    addMiddle(name, treshold, slope)
    n1.connect(neurons(currentNeuronId.get), weight)
    this
  }
  def chainMiddle():NetBuilder = chainMiddle(generateName(NetBuilder.MIDDLE_LAYER))
  def chainMiddle(weight: Double):NetBuilder = chainMiddle(generateName(NetBuilder.MIDDLE_LAYER), weight)
  def chainMiddle(weight: Double, treshold: Double):NetBuilder = chainMiddle(generateName(NetBuilder.MIDDLE_LAYER), weight, treshold)
  def chainMiddle(weight: Double, treshold: Double, sl: Double):NetBuilder = 
    chainMiddle(generateName(NetBuilder.MIDDLE_LAYER), weight, treshold, sl)
  
  def chainOutput(name: String, weight: Double =defWeight, treshold: Double =0.0):NetBuilder = {
    val n1 = current
    if(outs.contains(n1.id))
      throw new IllegalArgumentException("You can chain a new neuron in the output layer only to input or middle neurons")
    addOutput(name, treshold)
    n1.connect(neurons(currentNeuronId.get), weight)
    this
  }
  def chainOutput():NetBuilder = chainOutput(generateName(NetBuilder.OUTPUT_LAYER))
  def chainOutput(weight: Double):NetBuilder = chainOutput(generateName(NetBuilder.OUTPUT_LAYER), weight)
  def chainOutput(weight: Double, treshold: Double):NetBuilder = chainOutput(generateName(NetBuilder.OUTPUT_LAYER), weight, treshold)
  
  def loop(name: String, w1: Double =defWeight, treshold: Double =defTreshold, w2: Double =defWeight):NetBuilder = {
    val n1 = current
    if(!mids.contains(n1.id))
      throw new IllegalArgumentException("You can loop only in the middle layer")
    chainMiddle(name, w1, treshold)
    current.connect(n1, w2)
    currentNeuronId = Some(n1.id)
    this
  }
  
  def loop():NetBuilder = loop(generateName(NetBuilder.MIDDLE_LAYER))
  def loop(w1: Double, treshold: Double, w2: Double):NetBuilder = loop(generateName(NetBuilder.MIDDLE_LAYER), w1, treshold, w2)
  def loop(w1: Double, w2: Double):NetBuilder = loop(generateName(NetBuilder.MIDDLE_LAYER), w1, defTreshold, w2)
  
  def build:Net = {
    val net = Net(defSlope, defTreshold, defWeight)
    neurons.foreach(tuple => net.addNeuron(tuple._2))
    net.setInputLayer(ins.toSeq)
    net.setOutputLayer(outs.toSeq)
    net
  }
  
  def build(netInputName: String, netOutputName: String):(NetInput,Net,NetOutput) = {
    val net = build
    (NetInput(netInputName, net), net, NetOutput(netOutputName, net))
  }
}

object NetBuilder {
  val SLOPE = 20.0
  val TRESHOLD = 0.5
  val WEIGHT = 1.0
  
  val INPUT_LAYER = "in"
  val MIDDLE_LAYER = "mid"
  val OUTPUT_LAYER = "out"
  
  def apply() = new NetBuilder()
}