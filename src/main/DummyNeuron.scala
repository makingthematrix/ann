package main

final class DummyNeuron(id: String, treshold: Double = 0.0) extends Neuron(id,treshold) {
  override def calculateOutput = buffer
  
  override def tick() = {
    println(s"--- $id tick")
    if(buffer > treshold) run()
    buffer = 0.0 // buffer is cleared no matter if it was processed or not
    afterTickTriggers.values.foreach( _(this) )
  }
}

object DummyNeuron {
  def apply(id: Long, treshold: Double) = id match {
    case x if x <= 0L => new DummyNeuron("dummy_"+x, treshold)
    case x => throw new IllegalArgumentException("Only non-positive ids are allowed for dummy neurons")
  }
  def apply(id: String, treshold: Double) = new DummyNeuron(id, treshold)
  //def apply(treshold: Double) = new DummyNeuron("dummy_0", treshold)
  def apply() = new DummyNeuron("dummy_0",0.0)
}