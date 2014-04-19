package main

import scala.collection.mutable

case class Synapse(val destination: Neuron,var weight: Double){
  def send(signal: Double) = {
    val t = signal * weight
    println(s"sending signal $signal through synapse with weight $weight to neuron ${destination.id} -> $t")
    destination += t
  }
}

class Neuron(val id: Long, val treshold: Double = 0.5, val slope: Double = 20.0){
  protected val synapses = mutable.ListBuffer[Synapse]()
  protected var buffer = 0.0
  
  def input = buffer // only for debugging purposes
  
  def output = buffer match {
    case x if x <= 0.0 => 0.0
    case x if x >= 1.0 => 1.0
    case x => 1.0/(1.0+Math.exp(-slope*(x-0.5)));
  }
    // = 2/(1+EXP(-C*x))-1 ; mapowanie S -1->-1,0->0,1->1, gdzie C to stromość
    // = 1/(1+EXP(-C*(x-0.5))) ; mapowanie S 0->0,0.5->0.5,1->1, gdzie C to stromość
  
  def tick() = {
    println(s"--- $id tick with buffer $buffer and treshold $treshold")
    if(buffer > treshold) run()
    println(s"n$id, after tick: buffer = $buffer")
  }
   
  protected def run(){
    val output = this.output
    println(s"output $output")
    synapses foreach { _.send(output) } 
    afterTickTriggers.values.foreach( _(this) )
    buffer = 0.0
  }  
  
  def +=(signal: Double) = {
    buffer += signal
    if(buffer > treshold) tresholdPassedTriggers.values.foreach( _(this) )
  }
  
  def connect(destination:Neuron, weight: Double) = findSynapse(destination) match {
    case Some(s) => false
    case None => synapses += Synapse(destination, weight); true
  }
  
  def disconnect(destination: Neuron) = findSynapse(destination) match {
    case Some(s) => synapses -= s
    case None =>
  }
  
  def findSynapse(destination: Neuron) = synapses.find(_.destination.id == destination.id)
  
  def weightSum = synapses.map(_.weight).sum

  def averageWeight = weightSum / synapses.size

  def normalize = {
    val ws = weightSum
    synapses.foreach( _.weight /= ws )
  }

  def isPositive = !synapses.exists( _.weight < 0.0 )
  def isNegative = !synapses.exists( _.weight > 0.0 )
  def isMixed = synapses.exists( _.weight < 0.0 ) && synapses.exists( _.weight > 0.0 )

  private val afterTickTriggers = mutable.Map[String,(Neuron)=>Unit]()
  def addAfterTickTrigger(id: String, f: (Neuron) => Unit) = afterTickTriggers.contains(id) match {
    case false => afterTickTriggers.put(id, f)
    case true => throw new IllegalArgumentException(s"There was already registered a trigger with id $id")
  } 
  def isAfterTickTrigger(id: String) = afterTickTriggers.contains(id)
  def removeAfterTickTrigger(id: String) = afterTickTriggers.remove(id)
  def clearAfterTickTriggers() = afterTickTriggers.clear
  
  private val tresholdPassedTriggers = mutable.Map[String,(Neuron)=>Unit]()
  def addTresholdPassedTrigger(id: String, f: (Neuron) => Unit) = tresholdPassedTriggers.contains(id) match {
    case false => tresholdPassedTriggers.put(id, f)
    case true => throw new IllegalArgumentException("There was already registered a trigger with id " + id)
  } 
  def isTresholdPassedTrigger(id: String) = tresholdPassedTriggers.contains(id)
  def removeTresholdPassedTrigger(id: String) = tresholdPassedTriggers.remove(id)
  def clearTresholdPassedTriggers() = tresholdPassedTriggers.clear
}

object Neuron{
  private var serialId = 1L
  
  def getSerialId = serialId

  def apply() = {
    val n = new Neuron(serialId)
    serialId += 1
    n
  }
  
  def apply(treshold: Double, slope: Double) = {
    val n = new Neuron(serialId, treshold, slope)
    serialId += 1
    n
  }
  
}