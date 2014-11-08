package main.async

import akka.actor.ActorContext
import main.async.Messages.DontForget
import main.logger.LOG._
import main.async.Messages.Success

class DummyNeuron(override val id: String) extends Neuron(id,0.0,0.0,DontForget) {
  override protected def calculateOutput:Double = buffer
    
  override protected def init(usePresleep: Boolean){
    addTresholdPassedTrigger("run", () => run() )
    if(usePresleep) context.become(presleep)
    answer(Success("init_"+this.id))
  } 
}