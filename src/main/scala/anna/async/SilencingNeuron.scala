package anna.async

import anna.async.Messages._
import anna.data.SilenceIterations
import anna.logger.LOG

class SilencingNeuron(override val id: String, override val netId: String)
extends Neuron(id, netId, 0.0, SilenceIterations()) {
  private def sendSilenceRequest() = {
    synapses.foreach( s => {
      LOG += s"sending SilenceRequest to ${s.dest.id}"
      s.dest.requestSilence()
    })
    triggerSilenceRequested()
  }
  
  override val activeBehaviour: Receive = {
    case Signal(s, id) =>
      LOG += s"$id signal received: $s"
      sendSilenceRequest()
    case SilenceRequest =>
      LOG += s"$id Silence" +
        s"Request received"
      sendSilenceRequest()
  }
}