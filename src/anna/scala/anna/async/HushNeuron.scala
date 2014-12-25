package anna.async

import akka.actor.ActorContext
import anna.async.Messages._
import anna.async.logger.LOG
import anna.data.HushValue
import anna.data.ForgetAll

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