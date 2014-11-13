package main.async

import akka.actor.ActorContext
import main.logger.LOG._
import main.async.Messages.Success

class DummyNeuron(override val id: String, hushValue: HushValue = HushValue()) extends Neuron(id, 0.0, 0.0, hushValue, ForgetAll) {
  override protected def calculateOutput:Double = buffer
    
  override protected def init(){
    addTresholdPassedTrigger("run", () => run() )
    answer(Success("init_"+this.id))
  } 
}