package main

final class DummyNeuron(override val id: Long, override val treshold: Double = 0.0) extends Neuron(id,treshold) {
  override def output = buffer
  
  override def tick() = {
    println(s"--- $id tick")
    if(buffer > treshold) run()
    buffer = 0.0 // buffer is cleared no matter if it was processed or not
  }
}

object DummyNeuron {
  def apply(id: Long, treshold: Double =0.0) = id match {
    case x if x <= 0L => new DummyNeuron(x, treshold)
    case x => throw new IllegalArgumentException("Only non-positive ids are allowed for dummy neurons")
  }
  def apply(treshold: Double) = new DummyNeuron(0L, treshold)
  def apply() = new DummyNeuron(0L)
}