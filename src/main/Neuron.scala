package main

import scala.collection.mutable

case class Synapse(val destination: Neuron,var weight: Double){
  def send(signal: Double) = destination += signal * weight
}

class Neuron(val id: Long, val slope: Double = 20.0,val hardTreshold: Double = 0.5){
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
  
  def tick() = if(buffer >= hardTreshold) run()
   
  protected def run(){
    val output = this.output
    synapses foreach { _.send(output) } 
    buffer = 0.0
  }  
  
  def +=(signal: Double) = {
    buffer += signal
    if(htpEvent != null && buffer >= hardTreshold) htpEvent(this)
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

  private var htpEvent:(Neuron)=>Unit = null

  def setHardTresholdPassedEvent(f: (Neuron) => Unit){
    htpEvent = f
  } 
  
  def clearHardTresholdPassedEvent(){
    htpEvent = null
  }
}

object Neuron{
  private var serialId = 1L
  
  def getSerialId = serialId

  def apply() = {
    val n = new Neuron(serialId)
    serialId += 1
    n
  }
  
  def apply(slope: Double, hardTreshold: Double) = {
    val n = new Neuron(serialId, slope, hardTreshold)
    serialId += 1
    n
  }
  
}