package main.async

import scala.collection.mutable
import akka.actor.ActorSystem
import main.logger.LOG._
import scala.concurrent.Await
import akka.util.Timeout
import scala.concurrent.duration._
import Messages._
import main.utils.Utils.await
import Context._

class AkkaNetBuilder {
  var defSlope = SLOPE
  var defTreshold = TRESHOLD
  var defWeight = WEIGHT
  var defForgetting = FORGETTING
  var resolution = 1
  var defInputName = INPUT_LAYER_NAME
  var defMiddleName = MIDDLE_LAYER_NAME
  var defOutputName = OUTPUT_LAYER_NAME
  var defNetName = "net"
  
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
      case INPUT_LAYER_NAME => defInputName
      case MIDDLE_LAYER_NAME => defMiddleName
      case OUTPUT_LAYER_NAME => defOutputName
    }
    
    prefix + nextId()
  }
  
  protected def newNeuron(id: String, treshold: Double, 
                        slope: Double, forgetting: Double) = 
    await[NeuronRef](net, CreateNeuron(id, treshold, slope, forgetting))
    
  protected def newInput(id: String, treshold: Double, slope: Double, forgetting: Double) = newNeuron(id, treshold, slope, forgetting)
  protected def newMiddle(id: String, treshold: Double, slope: Double, forgetting: Double) = newNeuron(id, treshold, slope, forgetting)
  protected def newOutput(id: String, treshold: Double, slope: Double, forgetting: Double) = newNeuron(id, treshold, slope, forgetting)

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

  private def add(name: String, layer: mutable.Set[String], treshold: Double, slope:Double, forgetting: Double):AkkaNetBuilder = {
    debug(this, s"adding $name with treshold $treshold, slope $slope and forgetting $forgetting")
    val n = if(contains(name)){
      if(!throwOnError) get(name) else throw new IllegalArgumentException(s"There is already a neuron with name $name")
    } else add(newNeuron(name, treshold, slope, forgetting)) 
            
    layer += n.id
    this    
  }
  
  private def chain(name: String, layer: mutable.Set[String], weight: Double, treshold: Double, 
                    slope: Double, forgetting: Double):AkkaNetBuilder = {
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
  def addInput():AkkaNetBuilder = addInput(generateName(INPUT_LAYER_NAME))
  
  def addMiddle(name: String, treshold: Double =defTreshold, 
                slope: Double = defSlope, forgetting: Double = defForgetting):AkkaNetBuilder = 
    add(name, mids, treshold, slope, forgetting)
  def addMiddle():AkkaNetBuilder = addInput(generateName(MIDDLE_LAYER_NAME))

  def chainMiddle(name: String, weight: Double =defWeight, treshold: Double =defTreshold, 
                  slope: Double =defSlope, forgetting: Double =defForgetting):AkkaNetBuilder = 
    chain(name, mids, weight, treshold, slope, forgetting)
  def chainMiddle():AkkaNetBuilder = chainMiddle(generateName(MIDDLE_LAYER_NAME))
  def chainMiddle(weight: Double):AkkaNetBuilder = chainMiddle(generateName(MIDDLE_LAYER_NAME), weight)
  def chainMiddle(weight: Double, treshold: Double):AkkaNetBuilder = 
    chainMiddle(generateName(MIDDLE_LAYER_NAME), weight, treshold)
  def chainMiddle(weight: Double, treshold: Double, slope: Double):AkkaNetBuilder = 
    chainMiddle(generateName(MIDDLE_LAYER_NAME), weight, treshold, slope)
    
  def addOutput(name: String, treshold: Double =defTreshold, 
                slope: Double = defSlope, forgetting: Double = defForgetting):AkkaNetBuilder = 
    add(name, outs, treshold, slope, forgetting)
  def addOutput():AkkaNetBuilder = addInput(generateName(OUTPUT_LAYER_NAME))
  def chainOutput(name: String, weight: Double =defWeight, treshold: Double =defTreshold, 
                  slope: Double =defSlope, forgetting: Double =defForgetting):AkkaNetBuilder = 
    chain(name, outs, weight, treshold, slope, forgetting)
  def chainOutput():AkkaNetBuilder = chainOutput(generateName(OUTPUT_LAYER_NAME))
  def chainOutput(weight: Double):AkkaNetBuilder = chainOutput(generateName(OUTPUT_LAYER_NAME), weight)
  def chainOutput(weight: Double, treshold: Double):AkkaNetBuilder = 
    chainOutput(generateName(OUTPUT_LAYER_NAME), weight, treshold)
    
  def loop(name: String, w1: Double =defWeight, treshold: Double =defTreshold, w2: Double =defWeight, slope: Double =defSlope):AkkaNetBuilder = {
    val n1 = current
    if(throwOnError && !mids.contains(n1.id))
      throw new IllegalArgumentException("You can loop only in the middle layer")
    chainMiddle(name, w1, treshold, slope)
    current.connect(n1, w2)
    currentNeuronId = Some(n1.id)
    this
  }
    
  def loop():AkkaNetBuilder = loop(generateName(MIDDLE_LAYER_NAME))
  def loop(w1: Double, treshold: Double, w2: Double):AkkaNetBuilder = loop(generateName(MIDDLE_LAYER_NAME), w1, treshold, w2)
  def loop(w1: Double, w2: Double):AkkaNetBuilder = loop(generateName(MIDDLE_LAYER_NAME), w1, defTreshold, w2)
  
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
  def apply():AkkaNetBuilder = {
    debug("a new akka net builder created")
    new AkkaNetBuilder()
  }
}