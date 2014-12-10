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
}
