package anna.async

import anna.data.SilenceIterations

class DummyNeuron(override val id: String, override val netId: String)
extends Neuron(id, netId, 0.0, SilenceIterations(0)) {
}