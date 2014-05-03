package main

import Utils._

class DelayNeuron(id: String, treshold: Double = 0.5, slope: Double = 20.0, forgetting: Double = 0.0) 
extends Neuron(id, treshold, slope, forgetting) {
  protected var lastTickBuffer = 0.0
  override def tick() = {
    println(s"--- $id tick with buffer $lastTickBuffer and treshold $treshold")
    if(lastTickBuffer > treshold) run()
    
    if(buffer > 0.0) tickForgetting()

    lastTickBuffer = buffer
 
    afterTickTriggers.values.foreach( _(this) )
  }
  
  def getLastTickBuffer = lastTickBuffer // debugging purposes only
  
  override protected def calculateOutput = lastTickBuffer match {
    case x if x <= 0.0 => 0.0
    case x if x >= 1.0 => 1.0
    case x => 1.0/(1.0+Math.exp(-slope*(x-0.5)));
  }
}

object DelayNeuron{
  private var serialId = 1L
  
  def getSerialId = serialId

  def apply():DelayNeuron = {
    val n = new DelayNeuron("delay_"+serialId)
    serialId += 1
    n
  }
  
  def apply(treshold: Double, slope: Double):DelayNeuron = {
    val n = new DelayNeuron("delay_"+serialId, treshold, slope)
    serialId += 1
    n
  }
  
  def apply(id: Long, treshold: Double, slope: Double):DelayNeuron = {
    val n = new DelayNeuron("delay_"+id, treshold, slope)
    if(serialId <= id) serialId = id + 1
    n
  }
  
  def apply(id: String, treshold: Double, slope: Double):DelayNeuron = new DelayNeuron(id, treshold, slope)
  
  def apply(id: String, treshold: Double, slope: Double, forgetting: Double):DelayNeuron = new DelayNeuron(id, treshold, slope, forgetting)
  
  
  def apply(name: String, n: DelayNeuron):DelayNeuron = {
    val newN = new DelayNeuron(name, n.treshold, n.slope, n.forgetting)
    n.synapses.foreach( s => newN.connect(s.destination,s.weight) )
    newN
  }
  
  def apply(n: DelayNeuron):DelayNeuron = apply(n.id, n)
}