package main.async

import akka.actor.ActorSystem
import akka.util.Timeout
import scala.concurrent.duration._
import main.async.Messages._

object Context {
  type Trigger = () => Any
    
  implicit val timeout = Timeout(5 seconds)
  val system = ActorSystem("system")
  
  // neuron
  val slope = 20.0
  val threshold = 0.5
  val weight = 1.0
  val hushValue = HushValue(0.0)
  val forgetting = DontForget
  val sleepTime = 50L
  val forgettingGranularity = 1.0
  
  val INPUT_LAYER_NAME = "in"
  val MIDDLE_LAYER_NAME = "mid"
  val OUTPUT_LAYER_NAME = "out"
    
}