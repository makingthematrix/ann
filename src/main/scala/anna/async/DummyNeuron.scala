package anna.async

import anna.data.HushValue

class DummyNeuron(override val id: String, override val netId: String)
extends Neuron(id, netId, 0.0, HushValue()) {
  override protected def calculateOutput:Double = buffer
}