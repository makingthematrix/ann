package anna.async

import anna.async.Messages._
import anna.data.{ForgetAll, HushValue}

class HushNeuron(override val id: String, override val netId: String)
extends Neuron(id, netId, 0.0, 0.0, HushValue(), ForgetAll(), 1.0, ActivationFunction(ActivationFunction.UNUSED)) {
  private def sendHush() = {
    synapses.foreach( _.dest.hush())
    triggerHushRequested()
  }
  
  override val activeBehaviour: Receive = {
    case Signal(s) => sendHush()
    case HushNow => sendHush()
    case WakeUp =>
  }
}