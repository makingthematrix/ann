package anna.async

import anna.data.{ForgetAll, HushValue}

class DummyNeuron(override val id: String, override val tickTime: Long)
extends Neuron(id, 0.0, 0.0, HushValue(), ForgetAll, tickTime) {
  override protected def calculateOutput:Double = buffer
}