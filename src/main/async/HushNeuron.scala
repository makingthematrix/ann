package main.async

import akka.actor.ActorContext
import main.async.Messages._
import main.async.logger.LOG
import main.data.HushValue
import main.data.ForgetAll

class HushNeuron(override val id: String) 
extends Neuron(id, 0.0, 0.0, HushValue(), ForgetAll) {
  private def sendHush() = {
    synapses.foreach( _.dest ! HushNow)
    triggerHushRequested()
  }
  
  override val activeBehaviour: Receive = {
    case Signal(s) => sendHush()
    case HushNow => sendHush()
    case WakeUp =>
  }
}