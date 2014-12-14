package main.async

import akka.actor.ActorContext
import main.async.Messages._
import main.async.logger.LOG

class HushNeuron(override val id: String) 
extends Neuron(id, 0.0, 0.0, HushValue(), ForgetAll) {

  private def sendHush() = {
    synapses.foreach( _.dest ! HushNow)
    triggerHushRequested()
  }
  
  override protected def init(){
    LOG += s"init for the hush neuron $id" 
    answer(Success("init_"+this.id))
  }
  
  override val activeBehaviour: Receive = {
    case Signal(s) => sendHush()
    case HushNow => sendHush()
    case WakeUp =>
  }
  
  
}