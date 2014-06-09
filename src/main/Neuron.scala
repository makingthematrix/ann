package main

import scala.collection.mutable
import Utils._

case class Synapse(val source: Neuron, val destination: Neuron,var weight: Double){
  def send(signal: Double) = {
    val t = signal * weight
    LOG.log( s"sending signal $signal through synapse with weight $weight from ${source.id} to ${destination.id} -> $t", source)
    destination += t
  }
}

class Neuron(val id: String, val treshold: Double =0.5, val slope: Double =20.0, var forgetting: Double =0.0, var priority: Int =0){
  implicit val self = this 
  
  protected val synapses = mutable.ListBuffer[Synapse]()
  protected var buffer = 0.0
  protected var output = 0.0
  
  def input = buffer // only for debugging purposes
  def lastOutput = output // only for debugging purposes
  
  def rewire(n1: Neuron, n2: Neuron) = synapses.filter( _.destination == n1 ).foreach( s => {
    synapses -= s
    synapses += Synapse(this, n2, s.weight)
  })
  
  def silence(){
    buffer = 0.0
    output = 0.0
  }
  
  protected def calculateOutput = minmax(buffer, 0.0, 1.0, 1.0/(1.0+Math.exp(-slope*(buffer-0.5))) )
    // = 2/(1+EXP(-C*x))-1 ; mapowanie S -1->-1,0->0,1->1, gdzie C to stromość
    // = 1/(1+EXP(-C*(x-0.5))) ; mapowanie S 0->0,0.5->0.5,1->1, gdzie C to stromość
  
  protected def tickForgetting() = 
    if(buffer > 0.0) buffer = Math.max(buffer - forgetting, 0.0)
    else if(buffer < 0.0) buffer = Math.min(buffer + forgetting, 0.0)
     // might be changed into the S function later on
  
  def getSynapses = synapses.toList
  
  def tick() = {
    LOG += s"--- $id tick with buffer $buffer and treshold $treshold"

    if(buffer > treshold) run()
    else if(buffer > 0.0){ 
      tickForgetting()
      output = 0.0
    }
    
    LOG +=  s"$id, after tick: buffer = $buffer"
    afterTickTriggers.values.foreach( _(this) )
  }
   
  protected def run(){
    output = calculateOutput
    buffer = 0.0
    //println(s"output $output")
    synapses foreach { _.send(output) } 
    afterFireTriggers.values.foreach( _(this) )
  }  
  
  def +=(signal: Double) = {
    buffer = minmax(-1.0, buffer+signal, 1.0)
    if(buffer > treshold) tresholdPassedTriggers.values.foreach( _(this) )
  }
  
  def connect(destination:Neuron, weight: Double) = findSynapse(destination) match {
    case Some(s) => false
    case None => synapses += Synapse(this, destination, weight); true
  }
  
  def disconnect(destination: Neuron) = findSynapse(destination) match {
    case Some(s) => synapses -= s
    case None =>
  }
  
  def findSynapse(destination: Neuron) = synapses.find(_.destination.id == destination.id)
  
  def weightSum = synapses.map(_.weight).sum
  def absWeightSum = synapses.map( s => math.abs(s.weight) ).sum

  def averageWeight = weightSum / synapses.size

  def normalize = {
    val ws = weightSum
    synapses.foreach( _.weight /= ws )
  }

  def isPositive = !synapses.exists( _.weight < 0.0 )
  def isNegative = !synapses.exists( _.weight > 0.0 )
  def isMixed = synapses.exists( _.weight < 0.0 ) && synapses.exists( _.weight > 0.0 )

  protected val afterFireTriggers = mutable.Map[String,(Neuron)=>Any]()
  def addAfterFireTrigger(id: String, f: (Neuron) => Any) = afterFireTriggers.contains(id) match {
    case false => afterFireTriggers.put(id, f)
    case true => throw new IllegalArgumentException(s"There was already registered an after fire trigger with id $id")
  } 
  def isAfterFireTrigger(id: String) = afterFireTriggers.contains(id)
  def removeAfterFireTrigger(id: String) = afterFireTriggers.remove(id)
  def clearAfterFireTriggers() = afterFireTriggers.clear

  protected val afterTickTriggers = mutable.Map[String,(Neuron)=>Any]()
  def addAfterTickTrigger(id: String, f: (Neuron) => Any) = afterTickTriggers.contains(id) match {
    case false => afterTickTriggers.put(id, f)
    case true => throw new IllegalArgumentException(s"There was already registered a after tick trigger with id $id")
  } 
  def isTickFireTrigger(id: String) = afterTickTriggers.contains(id)
  def removeAfterTickTrigger(id: String) = afterTickTriggers.remove(id)
  def clearAfterTickTriggers() = afterTickTriggers.clear
  
  protected val tresholdPassedTriggers = mutable.Map[String,(Neuron)=>Any]()
  def addTresholdPassedTrigger(id: String, f: (Neuron) => Any) = tresholdPassedTriggers.contains(id) match {
    case false => tresholdPassedTriggers.put(id, f)
    case true => throw new IllegalArgumentException("There was already registered a trigger with id " + id)
  } 
  def isTresholdPassedTrigger(id: String) = tresholdPassedTriggers.contains(id)
  def removeTresholdPassedTrigger(id: String) = tresholdPassedTriggers.remove(id)
  def clearTresholdPassedTriggers() = tresholdPassedTriggers.clear
  
  def clearAllTriggers() = {
    clearAfterFireTriggers()
    clearAfterTickTriggers()
    clearTresholdPassedTriggers()
  }
  
  def copy(_id: String =id, _treshold: Double =treshold, _slope: Double =slope, _forgetting: Double =forgetting) = {
    val newN = new Neuron(_id, _treshold, _slope, _forgetting)
    this.synapses.foreach( s => newN.connect(s.destination,s.weight) )
    newN
  }
}

object Neuron{
  private var _serialId = 1L
  
  def serialId = _serialId

  def apply():Neuron = {
    val n = new Neuron("neuron_"+_serialId)
    _serialId += 1
    n
  }
  
  def apply(treshold: Double, slope: Double):Neuron = {
    val n = new Neuron("neuron_"+_serialId, treshold, slope)
    _serialId += 1
    n
  }
  
  def apply(id: Long, treshold: Double, slope: Double):Neuron = {
    val n = new Neuron("neuron_"+id, treshold, slope)
    if(_serialId <= id) _serialId = id + 1
    n
  }
  
  def apply(id: String, treshold: Double, slope: Double):Neuron = new Neuron(id, treshold, slope)
  
  def apply(id: String, treshold: Double, slope: Double, forgetting: Double):Neuron = new Neuron(id, treshold, slope, forgetting)
  
  def apply(name: String, n: Neuron):Neuron = {
    val newN = new Neuron(name, n.treshold, n.slope, n.forgetting)
    n.synapses.foreach( s => newN.connect(s.destination,s.weight) )
    newN
  }
  
  def apply(n: Neuron):Neuron = apply(n.id, n)
}