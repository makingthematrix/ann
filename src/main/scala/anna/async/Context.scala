package anna.async

import akka.actor.ActorSystem
import akka.util.Timeout
import scala.concurrent.duration._
import anna.async.Messages._
import anna.data.HushValue
import anna.data.DontForget

object Context {    
  implicit val timeout = Timeout(5 seconds)
  val system = ActorSystem("system")
  
  // neuron
  val slope = 5.0
  val threshold = 0.5
  val weight = 1.0
  val hushValue = HushValue()
  val forgetting = DontForget
  val tickTime = 30L
  
  val INPUT_LAYER_NAME = "in"
  val MIDDLE_LAYER_NAME = "mid"   
}