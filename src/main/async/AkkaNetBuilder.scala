package main.async

import scala.collection.mutable
import akka.actor.ActorSystem
import main.logger.LOG._
import scala.concurrent.Await
import akka.util.Timeout
import scala.concurrent.duration._

class AkkaNetBuilder {
  implicit val timeout = Timeout(5 seconds)
  
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
  protected lazy val net = NetRef(defNetName, defSlope, defTreshold, defWeight)
  
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
                        slope: Double =defSlope, forgetting: Double = defForgetting):NeuronRef = {
    val answer = net ? CreateNeuron(id, treshold, slope, forgetting)
    Await.result(answer, timeout.duration).asInstanceOf[NeuronRef]
  } 
    //NeuronRef(id, treshold, slope, forgetting)
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
    debug(this, s"adding $name with treshold $treshold, slope $slope and forgetting $forgetting")
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
    debug(this, s"chaining from ${n1.id} to $name with treshold $treshold and slope $slope")
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
  def chainOutput(name: String, weight: Double =defWeight, treshold: Double =defTreshold, 
                  slope: Double =defSlope, forgetting: Double =defForgetting):AkkaNetBuilder = 
    chain(name, outs, weight, treshold, slope, forgetting)
  def chainOutput():AkkaNetBuilder = chainOutput(generateName(AkkaNetBuilder.OUTPUT_LAYER))
  def chainOutput(weight: Double):AkkaNetBuilder = chainOutput(generateName(AkkaNetBuilder.OUTPUT_LAYER), weight)
  def chainOutput(weight: Double, treshold: Double):AkkaNetBuilder = 
    chainOutput(generateName(AkkaNetBuilder.OUTPUT_LAYER), weight, treshold)
    
  def loop(name: String, w1: Double =defWeight, treshold: Double =defTreshold, w2: Double =defWeight, slope: Double =defSlope):AkkaNetBuilder = {
    val n1 = current
    if(throwOnError && !mids.contains(n1.id))
      throw new IllegalArgumentException("You can loop only in the middle layer")
    chainMiddle(name, w1, treshold, slope)
    current.connect(n1, w2)
    currentNeuronId = Some(n1.id)
    this
  }
    
  def loop():AkkaNetBuilder = loop(generateName(AkkaNetBuilder.MIDDLE_LAYER))
  def loop(w1: Double, treshold: Double, w2: Double):AkkaNetBuilder = loop(generateName(AkkaNetBuilder.MIDDLE_LAYER), w1, treshold, w2)
  def loop(w1: Double, w2: Double):AkkaNetBuilder = loop(generateName(AkkaNetBuilder.MIDDLE_LAYER), w1, defTreshold, w2)
  
  def oscillator(name: String) = loop(name, 1.0, 0.5, -1.0)
  def oscillator() = loop(1.0, 0.5, -1.0)
  
  def chainOscillator(name: String, weight: Double, treshold: Double) = chainMiddle(name, weight, treshold).oscillator(name+"_osc")
  def chainOscillator(name: String, weight: Double) = chainMiddle(name, weight).oscillator(name+"_osc")
  def chainOscillator(weight: Double) = chainMiddle(weight).oscillator()
  
  
  def build:NetRef = {
    debug(this,s"build()")
    net ! SetInputLayer(ins.toSeq)
    debug(this, "input layer set request sent")
    net ! SetOutputLayer(outs.toSeq)
    debug(this, "output layer set request sent")
    net
  }
  
  def build(netInputName: String, netOutputName: String):(AkkaNetInput,NetRef,AkkaNetOutput) = {
    debug(this,s"build($netInputName,$netOutputName)")
    val in = AkkaNetInput(netInputName, net, resolution)
    debug(this, "net input built")
    val out = AkkaNetOutput(netOutputName, net)
    debug(this, "net output built")
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
    
  def apply():AkkaNetBuilder = {
    debug("a new akka net builder created")
    new AkkaNetBuilder()
  }
}