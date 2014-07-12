package main
import akka.actor._

class AkkaSynapse(val destinationId: String, val destinationRef: ActorRef, w: Double) extends Synapse(null,null,w)

