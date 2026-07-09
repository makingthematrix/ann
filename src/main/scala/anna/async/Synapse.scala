package anna.async

import anna.async.Messages.SynapseInfo
import anna.data.{Silence, SynapseTrait, SynapseWeight, Wake}

/**
 * A directed, weighted connection to a destination neuron ([[NeuronRef]]).
 * `send` translates the synapse weight into the appropriate [[Neuron.Command]]
 * (a `Signal` scaled by the weight, or a `SilenceRequest` / `WakeRequest`).
 */
final class Synapse(val dest: NeuronRef, val weight: SynapseTrait) {

  def send(signal: Double, senderId: String): Unit = weight match {
    case Silence()        => dest ! Neuron.SilenceRequest
    case Wake()           => dest ! Neuron.WakeRequest
    case w: SynapseWeight => dest ! Neuron.Signal(signal * w.weight, senderId)
  }

  def info: SynapseInfo = SynapseInfo(dest.id, weight)

  override def toString: String = s"Synapse(${dest.id}, $weight)"
}

object Synapse {
  def apply(dest: NeuronRef, weight: SynapseTrait): Synapse = new Synapse(dest, weight)
  def apply(dest: NeuronRef, weight: Double): Synapse = apply(dest, SynapseWeight(weight))
  def apply(dest: NeuronRef): Synapse = apply(dest, 1.0)
}
