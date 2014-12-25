package anna.async

import akka.actor.ActorContext
import anna.async.Messages.Success
import anna.data.ForgetAll
import anna.data.HushValue

class DummyNeuron(override val id: String, override val hushValue: HushValue) 
extends Neuron(id, 0.0, 0.0, hushValue, ForgetAll) {
  override protected def calculateOutput:Double = buffer
}