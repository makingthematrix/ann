package main.async

import akka.actor.ActorSystem
import akka.util.Timeout
import scala.concurrent.duration._

object Context {
  implicit val timeout = Timeout(5 seconds)
  val system = ActorSystem("system")
  
  val SLOPE = 20.0
  val TRESHOLD = 0.5
  val WEIGHT = 1.0
  val FORGETTING = 0.0
  
  val INPUT_LAYER_NAME = "in"
  val MIDDLE_LAYER_NAME = "mid"
  val OUTPUT_LAYER_NAME = "out"
    
}