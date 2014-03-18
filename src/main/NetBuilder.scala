package main

import scala.collection.mutable

class NetBuilder {
  var slope = NetBuilder.SLOPE
  var hardTreshold = NetBuilder.HARD_TRESHOLD
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
  
  def names = neuronNames.keySet
  
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
  
  def connect(name: String, weight: Double):NetBuilder = {
    current.connect(findByName(name), weight)
    this
  }
  def connect(name: String):NetBuilder = connect(name, defWeight)
  
  def addInput(name: String):NetBuilder = {
    val n = add(name, DummyNeuron(-nextId()))
    ins += n.id
    this
  }
  def addInput():NetBuilder = addInput(generateName(NetBuilder.INPUT_LAYER))
  
  def addMiddle(name: String, slope: Double, hardTreshold: Double):NetBuilder = {
    val n = add(name, new Neuron(nextId(), slope, hardTreshold))
    mids += n.id
    this
  }
  def addMiddle(name: String):NetBuilder = addMiddle(name, slope, hardTreshold)
  def addMiddle():NetBuilder = addMiddle(generateName(NetBuilder.MIDDLE_LAYER))
  
  def addOutput(name: String):NetBuilder = {
    val n = add(name, DummyNeuron(-nextId()))
    outs += n.id
    this
  }
  def addOutput():NetBuilder = addOutput(generateName(NetBuilder.OUTPUT_LAYER))
  
  def chainMiddle(name: String, weight: Double):NetBuilder = {
    val n1 = current
    if(outs.contains(n1.id))
      throw new IllegalArgumentException("You can chain a new neuron in the middle layer only to input or other middle neurons")
    addMiddle(name)
    n1.connect(neurons(currentNeuronId.get), weight)
    this
  }
  def chainMiddle(name: String):NetBuilder = chainMiddle(name, defWeight)
  def chainMiddle():NetBuilder = chainMiddle(generateName(NetBuilder.MIDDLE_LAYER))
  
  def chainOutput(name: String, weight: Double):NetBuilder = {
    val n1 = current
    if(outs.contains(n1.id))
      throw new IllegalArgumentException("You can chain a new neuron in the output layer only to input or middle neurons")
    addOutput(name)
    n1.connect(neurons(currentNeuronId.get), weight)
    this
  }
  def chainOutput(name: String):NetBuilder = chainOutput(name, defWeight)
  def chainOutput():NetBuilder = chainOutput(generateName(NetBuilder.OUTPUT_LAYER))
  
  def loop(name: String, w1: Double, w2: Double):NetBuilder = {
    val n1 = current
    if(!mids.contains(n1.id))
      throw new IllegalArgumentException("You can loop only in the middle layer")
    chainMiddle(name, w1)
    current.connect(n1, w2)
    currentNeuronId = Some(n1.id)
    this
  }
  def loop(name: String):NetBuilder = loop(name, defWeight, defWeight)
  def loop():NetBuilder = loop(generateName(NetBuilder.MIDDLE_LAYER))
  
  def build = {
    val net = Net(slope, hardTreshold, defWeight)
    neurons.foreach(tuple => net.addNeuron(tuple._2))
    net.setInputLayer(ins.toSeq)
    net.setOutputLayer(outs.toSeq)
    net
  }
}

object NetBuilder {
  val SLOPE = 20.0
  val HARD_TRESHOLD = 0.5
  val WEIGHT = 1.0
  
  val INPUT_LAYER = "in"
  val MIDDLE_LAYER = "mid"
  val OUTPUT_LAYER = "out"
  
  def apply() = new NetBuilder()
}