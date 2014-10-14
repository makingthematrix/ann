package main.async

import Messages._
import scala.concurrent._
import ExecutionContext.Implicits.global

class SynapseWeight(val weight: Double)

object SynapseWeight {
  def apply(weight: Double) = new SynapseWeight(weight)
}

case object Hush extends SynapseWeight(-1.0)

class Synapse(val dest: NeuronRef, val weight: SynapseWeight, val delay: Option[Long] = None){
  private def msg(signal: Double) = weight match {
    case Hush => HushNow
    case w: SynapseWeight => Signal(signal * w.weight) 
  }
  
  private def _send(signal: Double) = dest ! msg(signal)
  
  def send(signal: Double) = delay match {
    case None => _send(signal)
    case Some(d) => future {
      Thread.sleep(d)
      _send(signal)
    }
  }
}
