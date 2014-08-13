package main
import akka.actor._

class AkkaSynapse(val destinationRef: AkkaRef, w: Double) extends Synapse(null,null,w)

