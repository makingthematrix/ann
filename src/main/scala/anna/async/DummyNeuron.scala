package anna.async

import akka.actor.typed.Behavior

/**
 * A dummy neuron is just a [[Neuron]] with threshold `0.0` and no silencing: it
 * fires on any incoming signal and simply forwards it. It is used for the
 * network's input neurons.
 *
 * Like [[SilencingNeuron]] this is a factory for the neuron's initial
 * behaviour rather than a subclass, since [[Neuron]] is a `Behavior`.
 */
object DummyNeuron {
  def apply(id: String, netId: String): Behavior[Neuron.Command] = Neuron(id, netId, 0.0, 0)
}
