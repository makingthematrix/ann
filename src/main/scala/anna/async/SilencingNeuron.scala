package anna.async

import anna.async.Messages._
import anna.data.Wake
import anna.logger.LOG

class SilencingNeuron(override val id: String, override val netId: String)
extends Neuron(id, netId, 0.0, 0) {
  private def sendRequest() = {
    synapses.foreach( s => s.weight match {
      case Wake() =>
        LOG += s"sending WakeUpRequest to ${s.dest.id}"
        s.dest.requestWakeUp()
      case _ =>
        LOG += s"sending SilenceRequest to ${s.dest.id}"
        s.dest.requestSilence()
    })
    triggerSilenceRequested()
  }
  
  override val activeBehaviour: Receive = {
    case Signal(s, id) => sendRequest()
    case SilenceRequest => sendRequest()
    case WakeRequest => sendRequest()
  }
}