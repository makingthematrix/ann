package anna.async

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.{ActorRef => ClassicActorRef, Props => ClassicProps}

/**
 * User-guardian behaviour for the application's (typed) `ActorSystem`.
 *
 * A typed `ActorSystem` always has a user guardian and, unlike the classic one,
 * offers no `actorOf` for creating top-level actors "from the outside". This
 * guardian fills that gap for both worlds: it spawns typed behaviours as its
 * children (like `SpawnProtocol`) and — for the still-present classic stack —
 * creates classic actors as its children via the classic adapter.
 *
 * Callers ask the guardian (the `ActorSystem` itself is its `ActorRef`) using
 * `AskPattern`; see [[NetRef]] (typed) and [[NetRef]] (classic).
 */
object Guardian {
  sealed trait Command

  final case class Spawn[T](behavior: Behavior[T], replyTo: ActorRef[ActorRef[T]]) extends Command
  final case class SpawnClassic(props: ClassicProps, replyTo: ActorRef[ClassicActorRef]) extends Command

  def apply(): Behavior[Command] = Behaviors.receive { (context, msg) =>
    msg match {
      case spawn: Spawn[t] =>
        spawn.replyTo ! context.spawnAnonymous(spawn.behavior)
        Behaviors.same
      case SpawnClassic(props, replyTo) =>
        replyTo ! context.toClassic.actorOf(props)
        Behaviors.same
    }
  }
}
