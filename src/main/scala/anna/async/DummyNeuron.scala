package anna.async

import anna.async.Neuron.InitialState

class DummyNeuron(override val id: String, override val netId: String, override val initialState: InitialState)
extends Neuron(id, netId, 0.0, 0, initialState) {
}