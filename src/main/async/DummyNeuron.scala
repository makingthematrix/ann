package main.async

import akka.actor.ActorContext
import main.async.Messages.Success

class DummyNeuron(override val id: String, override val hushValue: HushValue) 
extends Neuron(id, 0.0, 0.0, hushValue, ForgetAll) {
  override protected def calculateOutput:Double = buffer
}