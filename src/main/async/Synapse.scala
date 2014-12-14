package main.async

import Messages._
import scala.concurrent._
import ExecutionContext.Implicits.global

sealed trait SynapseTrait extends Any
case class SynapseWeight(weight: Double) extends AnyVal with SynapseTrait

case object Hush extends SynapseTrait

class Synapse(val dest: NeuronRef, val weight: SynapseTrait){
  private def msg(signal: Double) = weight match {
    case Hush => HushNow
    case w: SynapseWeight => Signal(signal * w.weight) 
  }
  
  def send(signal: Double) = dest ! msg(signal)
  
  override def toString() = s"Synapse(${dest.id}, $weight)"
}

object Synapse{
  def apply(dest: NeuronRef, weight: SynapseTrait):Synapse = new Synapse(dest, weight)
  def apply(dest: NeuronRef, weight: Double):Synapse = apply(dest, SynapseWeight(weight))
}