package main.async

import scala.collection.mutable
import akka.actor.ActorSystem
import main.logger.LOG._

class AkkaNetBuilder(val system: ActorSystem) {
  var defSlope = AkkaNetBuilder.SLOPE
  var defTreshold = AkkaNetBuilder.TRESHOLD
  var defWeight = AkkaNetBuilder.WEIGHT
  var resolution = 1
  var defInputName = "in"
  var defMiddleName = "mid"
  var defOutputName = "out"
  var defNetName = "net"
  var defForgetting = AkkaNetBuilder.FORGETTING
  
  var throwOnError = true
  
  protected val neurons = mutable.Map[String,NeuronRef]()
  
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
      case AkkaNetBuilder.INPUT_LAYER => defInputName
      case AkkaNetBuilder.MIDDLE_LAYER => defMiddleName
      case AkkaNetBuilder.OUTPUT_LAYER => defOutputName
    }
    
    prefix + nextId()
  }
  
  protected def newNeuron(id: String, treshold: Double =defTreshold, 
                        slope: Double =defSlope, forgetting: Double = defForgetting) = 
    NeuronRef(id, treshold, slope, forgetting, system)
  protected def newInput(id: String, treshold: Double =defTreshold, 
                        slope: Double =defSlope, forgetting: Double = defForgetting) = newNeuron(id, treshold, slope, forgetting)
  protected def newMiddle(id: String, treshold: Double =defTreshold, 
                        slope: Double =defSlope, forgetting: Double = defForgetting) = newNeuron(id, treshold, slope, forgetting)
  protected def newOutput(id: String, treshold: Double =defTreshold, 
                        slope: Double =defSlope, forgetting: Double = defForgetting) = newNeuron(id, treshold, slope, forgetting)
  
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
  
  protected def add(n: NeuronRef) = {
    neurons.put(n.id, n)
    currentNeuronId = Some(n.id)
    n
  }

  private def add(name: String, layer: mutable.Set[String], 
                  treshold: Double = defTreshold, slope:Double = defSlope, 
                  forgetting: Double = defForgetting):AkkaNetBuilder = {
    debug(this, s"add $name")
    val n = if(contains(name)){
      if(!throwOnError) get(name) else throw new IllegalArgumentException(s"There is already a neuron with name $name")
    } else add(newNeuron(name, treshold, slope, forgetting)) 
            
    layer += n.id
    this    
  }
  
  private def chain(name: String, layer: mutable.Set[String], 
                    weight: Double =defWeight, treshold: Double =defTreshold, 
                    slope: Double =defSlope, forgetting: Double = defForgetting):AkkaNetBuilder = {
    val n1 = current
    println(s"chaining from ${n1.id} to $name")
    if(throwOnError && outs.contains(n1.id))
      throw new IllegalArgumentException("You can chain a new neuron only from input or middle neurons")
    add(name, layer, treshold, slope, forgetting)
    val n = neurons(currentNeuronId.get)
    n1.connect(n, weight)
    this
  }
    
  def addInput(name: String, treshold: Double =0.0, slope:Double = defSlope, forgetting: Double = 0.0):AkkaNetBuilder = 
    add(name, ins, treshold, slope, forgetting)
  def addInput():AkkaNetBuilder = addInput(generateName(AkkaNetBuilder.INPUT_LAYER))
  
  def addMiddle(name: String, treshold: Double =defTreshold, 
                slope: Double = defSlope, forgetting: Double = defForgetting):AkkaNetBuilder = 
    add(name, mids, treshold, slope, forgetting)
  def addMiddle():AkkaNetBuilder = addInput(generateName(AkkaNetBuilder.MIDDLE_LAYER))

  def chainMiddle(name: String, weight: Double =defWeight, treshold: Double =defTreshold, 
                  slope: Double =defSlope, forgetting: Double =defForgetting):AkkaNetBuilder = 
    chain(name, mids, weight, treshold, slope, forgetting)
  def chainMiddle():AkkaNetBuilder = chainMiddle(generateName(AkkaNetBuilder.MIDDLE_LAYER))
  def chainMiddle(weight: Double):AkkaNetBuilder = chainMiddle(generateName(AkkaNetBuilder.MIDDLE_LAYER), weight)
  def chainMiddle(weight: Double, treshold: Double):AkkaNetBuilder = 
    chainMiddle(generateName(AkkaNetBuilder.MIDDLE_LAYER), weight, treshold)
  def chainMiddle(weight: Double, treshold: Double, slope: Double):AkkaNetBuilder = 
    chainMiddle(generateName(AkkaNetBuilder.MIDDLE_LAYER), weight, treshold, slope)
    
  def addOutput(name: String, treshold: Double =defTreshold, 
                slope: Double = defSlope, forgetting: Double = defForgetting):AkkaNetBuilder = 
    add(name, outs, treshold, slope, forgetting)
  def addOutput():AkkaNetBuilder = addInput(generateName(AkkaNetBuilder.OUTPUT_LAYER))
  def chainOutput(name: String, weight: Double =defWeight, treshold: Double =0.0, 
                  slope: Double =defSlope, forgetting: Double =defForgetting):AkkaNetBuilder = 
    chain(name, outs, treshold, slope, forgetting)
  def chainOutput():AkkaNetBuilder = chainOutput(generateName(AkkaNetBuilder.OUTPUT_LAYER))
  def chainOutput(weight: Double):AkkaNetBuilder = chainOutput(generateName(AkkaNetBuilder.OUTPUT_LAYER), weight)
  def chainOutput(weight: Double, treshold: Double):AkkaNetBuilder = 
    chainOutput(generateName(AkkaNetBuilder.OUTPUT_LAYER), weight, treshold)
  def build:NetRef = {
    val net = NetRef(defNetName, defSlope, defTreshold, defWeight, system)
    neurons.foreach(tuple => net ! AddNeuron(tuple._2))
    net ! SetInputLayer(ins.toSeq)
    net ! SetOutputLayer(outs.toSeq)
    net
  }
  
  def build(netInputName: String, netOutputName: String):(AkkaNetInput,NetRef,AkkaNetOutput) = {
    val net = build
    val in = AkkaNetInput(netInputName, net, resolution)
    val out = AkkaNetOutput(netOutputName, net)
    (in, net, out)
  }
}

object AkkaNetBuilder {
  val SLOPE = 20.0
  val TRESHOLD = 0.5
  val WEIGHT = 1.0
  val FORGETTING = 0.0
  
  val INPUT_LAYER = "in"
  val MIDDLE_LAYER = "mid"
  val OUTPUT_LAYER = "out"
    
  val system = ActorSystem("AkkaNeuronSystem")
  def apply():AkkaNetBuilder = apply(system)
  
  def apply(system: ActorSystem):AkkaNetBuilder = {
    println("a new akka net builder created")
    new AkkaNetBuilder(system)
  }
}