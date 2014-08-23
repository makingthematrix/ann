package main.sync

import main.utils.Utils._
import main.logger._

class DelayNeuron(id: String, treshold: Double = 0.5, slope: Double = 20.0, forgetting: Double = 0.0) 
extends Neuron(id, treshold, slope, forgetting) {
  protected var lastTickBuffer = 0.0
  override def tick() = {
    LOG += s"--- $id tick with buffer $lastTickBuffer and treshold $treshold"
    if(lastTickBuffer > treshold) run() else output = 0.0
    
    lastTickBuffer = buffer
    tickForgetting()
 
    afterTickTriggers.values.foreach( _(this) )
  }
  
  def getLastTickBuffer = lastTickBuffer // debugging purposes only
  
  override protected def calculateOutput = minMaxClosed(lastTickBuffer, 0.0, 1.0, 1.0/(1.0+Math.exp(-slope*(lastTickBuffer-0.5))) )
  
  override def copy(_id: String =id, _treshold: Double =treshold, _slope: Double =slope, _forgetting: Double =forgetting) = {
    val newN = new DelayNeuron(_id, _treshold, _slope, _forgetting)
    synapses.foreach( s => newN.connect(s.destination,s.weight) )
    newN
  }
}

object DelayNeuron {
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
  
  
  def apply(name: String, n: Neuron):DelayNeuron = {
    val newN = new DelayNeuron(name, n.treshold, n.slope, n.forgetting)
    n.getSynapses.foreach( s => newN.connect(s.destination,s.weight) )
    newN
  }
  
  def apply(n: DelayNeuron):DelayNeuron = apply(n.id, n)
}