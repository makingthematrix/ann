package anna.async

import anna.async.Messages._
import anna.data.HushValue
import anna.logger.LOG

class HushNeuron(override val id: String, override val netId: String)
extends Neuron(id, netId, 0.0, HushValue()) {
  private def sendHush() = {
    synapses.foreach( s => {
      LOG += s"sending hush to ${s.dest.id}"
      s.dest.hush()
    })
    triggerHushRequested()
  }
  
  override val activeBehaviour: Receive = {
    case Signal(s, id) =>
      LOG += s"$id signal received: $s"
      sendHush()
    case HushRequest =>
      LOG += s"$id HushRequest received"
      sendHush()
  }
}