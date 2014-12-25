package anna.async

import anna.data.{ForgetAll, HushValue}

class DummyNeuron(override val id: String, override val hushValue: HushValue) 
extends Neuron(id, 0.0, 0.0, hushValue, ForgetAll) {
  override protected def calculateOutput:Double = buffer
}