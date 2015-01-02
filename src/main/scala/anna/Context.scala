package anna

import akka.actor.ActorSystem
import akka.util.Timeout
import anna.data.{DontForget, HushValue}

import scala.concurrent.duration._

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

}