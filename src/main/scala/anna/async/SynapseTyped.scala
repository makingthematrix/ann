package anna.async

import anna.async.Messages.SynapseInfo
import anna.data.{Silence, SynapseTrait, SynapseWeight, Wake}

/**
 * Typed-Actor version of [[Synapse]].
 *
 * Identical in behaviour to the classic `Synapse`, but its destination is a
 * [[NeuronRefTyped]] and it sends the typed [[NeuronTyped.Command]] messages.
 * This is what lets a typed net connect its neurons (the classic `Synapse`
 * points at the untyped [[NeuronRef]] and can't target a [[NeuronTyped]]).
 */
final class SynapseTyped(val dest: NeuronRefTyped, val weight: SynapseTrait) {

  def send(signal: Double, senderId: String): Unit = weight match {
    case Silence()        => dest ! NeuronTyped.SilenceRequest
    case Wake()           => dest ! NeuronTyped.WakeRequest
    case w: SynapseWeight => dest ! NeuronTyped.Signal(signal * w.weight, senderId)
  }

  def info: SynapseInfo = SynapseInfo(dest.id, weight)

  override def toString: String = s"SynapseTyped(${dest.id}, $weight)"
}

object SynapseTyped {
  def apply(dest: NeuronRefTyped, weight: SynapseTrait): SynapseTyped = new SynapseTyped(dest, weight)
  def apply(dest: NeuronRefTyped, weight: Double): SynapseTyped = apply(dest, SynapseWeight(weight))
  def apply(dest: NeuronRefTyped): SynapseTyped = apply(dest, 1.0)
}
