package anna.async

import anna.async.Messages._
import anna.data.{Silence, SynapseTrait, SynapseWeight, Wake}

class Synapse(val dest: NeuronRef, val weight: SynapseTrait){

  private def msg(signal: Double, senderId: String) = weight match {
    case Silence() => SilenceRequest
    case Wake() => WakeRequest
    case w: SynapseWeight => Signal(signal * w.weight, senderId)
  }
  
  def send(signal: Double, senderId: String) = {
    dest ! msg(signal, senderId)
  }
  
  override def toString() = s"Synapse(${dest.id}, $weight)"

  def info = SynapseInfo(dest.id, weight)
}

object Synapse{
  def apply(dest: NeuronRef, weight: SynapseTrait):Synapse = new Synapse(dest, weight)
  def apply(dest: NeuronRef, weight: Double):Synapse = apply(dest, SynapseWeight(weight))
  def apply(dest: NeuronRef):Synapse = apply(dest, 1.0)
}