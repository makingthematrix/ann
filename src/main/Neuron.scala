package main

import scala.collection.mutable
import main.logger.LOG
import main.utils.Utils._

class Neuron(val id: String, val treshold: Double =0.5, val slope: Double =20.0, var forgetting: Double =0.0, var priority: Int =0) 
extends NeuronLike with NeuronTriggers[Neuron]{
  implicit val self = this 
  
  def getId = id
  
  protected val synapses = mutable.ListBuffer[Synapse]()
  protected var buffer = 0.0
  protected var output = 0.0
  
  def input = buffer // only for debugging purposes
  def lastOutput = output // only for debugging purposes
  
  def rewire(n1: Neuron, n2: Neuron) = synapses.filter( _.destination == n1 ).foreach( s => {
    synapses -= s
    synapses += new Synapse(this, n2, s.weight)
  })
  
  def silence(){
    buffer = 0.0
    output = 0.0
  }
  
  protected def calculateOutput = minMaxClosed(buffer, 0.0, 1.0, 1.0/(1.0+Math.exp(-slope*(buffer-0.5))) )
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
  
  def connect(destination:NeuronLike, weight: Double) = findSynapse(destination) match {
    case Some(s) => false
    case None => synapses += new Synapse(this, destination, weight); true
  }
  
  def disconnect(destination: NeuronLike) = findSynapse(destination) match {
    case Some(s) => synapses -= s
    case None =>
  }
  
  def findSynapse(destination: NeuronLike) = synapses.find(_.destination.getId == destination.getId)
  
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