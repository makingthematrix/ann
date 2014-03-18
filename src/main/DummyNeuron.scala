package main

final class DummyNeuron(override val id: Long) extends Neuron(id) {
  override def tick() = synapses foreach { _.send(buffer) } 
}

object DummyNeuron {
  def apply() = new DummyNeuron(0L)
  def apply(id: Long) = id match {
    case x if x <= 0L => new DummyNeuron(x)
    case x => throw new IllegalArgumentException("Only non-positive ids are allowed for dummy neurons")
  }
}