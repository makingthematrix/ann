package anna.async

import anna.data.{ForgetAll, HushValue}

class DummyNeuron(override val id: String, override val netId: String, override val tickTimeMultiplier: Double)
extends Neuron(id, netId, 0.0, 0.0, HushValue(), ForgetAll(), tickTimeMultiplier,
               ActivationFunction(ActivationFunction.UNUSED)) {
  override protected def calculateOutput:Double = buffer
}