package main.async

import akka.actor.ActorContext
import main.logger.LOG._
import main.async.Messages.Success
import main.async.Messages.ForgetAll

class DummyNeuron(override val id: String, hushValue: HushValue = HushValue(0.0)) extends Neuron(id, 0.0, 0.0, hushValue, ForgetAll) {
  override protected def calculateOutput:Double = buffer
    
  override protected def init(usePresleep: Boolean){
    addTresholdPassedTrigger("run", () => run() )
    //if(usePresleep) context.become(presleep)
    answer(Success("init_"+this.id))
  } 
}