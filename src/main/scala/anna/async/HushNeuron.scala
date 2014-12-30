package anna.async

import anna.async.Messages._
import anna.data.{ForgetAll, HushValue}

class HushNeuron(override val id: String) 
extends Neuron(id, 0.0, 0.0, HushValue(), ForgetAll, 0L) {
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