package anna.async

import anna.data.{ForgetAll, HushValue}

class DummyNeuron(override val id: String, override val tickTimeMultiplicity: Double)
extends Neuron(id, 0.0, 0.0, HushValue(), ForgetAll, tickTimeMultiplicity) {
  override protected def calculateOutput:Double = buffer
}