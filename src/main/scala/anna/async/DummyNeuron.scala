package anna.async

import anna.data.{ForgetAll, HushValue}

class DummyNeuron(override val id: String, override val tickTimeMultiplier: Double)
extends Neuron(id, 0.0, 0.0, HushValue(), ForgetAll(), tickTimeMultiplier) {
  override protected def calculateOutput:Double = buffer
}