package main.sync

import main.logger.LOG


final class DummyNeuron(id: String, treshold: Double = 0.0) extends Neuron(id,treshold) {
  override def calculateOutput = buffer
  
  override def tick(){
    LOG += s"--- $id tick"
    if(buffer > treshold) run()
    else output = 0.0
    buffer = 0.0 // buffer is cleared no matter if it was processed or not
    afterTickTriggers.values.foreach( _() )
  }
  
  override def copy(_id: String =id, _treshold: Double =treshold, _slope: Double =slope, _forgetting: Double =forgetting) = {
    val newN = new DummyNeuron(_id, _treshold)
    synapses.foreach( s => newN.connect(s.destination,s.weight) )
    newN
  }
}

object DummyNeuron {
  def apply(id: Long, treshold: Double) = id match {
    case x if x <= 0L => new DummyNeuron("dummy_"+x, treshold)
    case x => throw new IllegalArgumentException("Only non-positive ids are allowed for dummy neurons")
  }
  def apply(id: String, treshold: Double) = new DummyNeuron(id, treshold)
  def apply() = new DummyNeuron("dummy_0",0.0)
}