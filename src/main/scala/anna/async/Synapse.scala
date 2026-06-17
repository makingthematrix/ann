package anna.async

import anna.async.Messages._
import anna.data.{Silence, SynapseTrait, SynapseWeight, Wake}

final class Synapse(val dest: NeuronRef, val weight: SynapseTrait){

  private def msg(signal: Double, senderId: String): Message = weight match {
    case Silence() => SilenceRequest
    case Wake() => WakeRequest
    case w: SynapseWeight => Signal(signal * w.weight, senderId)
  }
  
  def send(signal: Double, senderId: String): Unit = {
    dest ! msg(signal, senderId)
  }
  
  override def toString: String = s"Synapse(${dest.id}, $weight)"

  def info: SynapseInfo = SynapseInfo(dest.id, weight)
}

object Synapse{
  def apply(dest: NeuronRef, weight: SynapseTrait):Synapse = new Synapse(dest, weight)
  def apply(dest: NeuronRef, weight: Double):Synapse = apply(dest, SynapseWeight(weight))
  def apply(dest: NeuronRef):Synapse = apply(dest, 1.0)
}