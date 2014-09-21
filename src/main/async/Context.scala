package main.async

import akka.actor.ActorSystem

object Context {
  val system = ActorSystem("system")
}