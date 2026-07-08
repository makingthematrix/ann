package anna.async

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import anna.async.NeuronTyped._
import anna.data.Wake

/**
 * Typed-Actor version of [[SilencingNeuron]].
 *
 * Like the classic one it is a [[NeuronTyped]] with threshold `0.0` and no
 * silencing of its own. It overrides the active-state handling so that any
 * incoming signal or silence/wake request is turned into Silence/Wake requests
 * forwarded to its synapse destinations (see [[SilencingNeuron]]).
 *
 * The queries and triggers of the base neuron (`GetData`, `GetSynapses`, the
 * trigger messages, `Reset`, ...) keep working through the inherited
 * `handleCommon` / `reset`, mirroring the classic
 * `activeBehaviour orElse commonBehaviour` composition.
 */
final class SilencingNeuronTyped(context: ActorContext[Command],
                                 timers: TimerScheduler[Command],
                                 override val id: String,
                                 override val netId: String,
                                 initialSynapses: List[SynapseTyped])
  extends NeuronTyped(context, timers, id, netId, 0.0, 0, initialSynapses) {

  private def sendRequest(): Unit = {
    synapses.foreach(s => s.weight match {
      case Wake() =>
        log(s"sending WakeUpRequest to ${s.dest.id}")
        s.dest.requestWakeUp()
      case _ =>
        log(s"sending SilenceRequest to ${s.dest.id}")
        s.dest.requestSilence()
    })
    triggerSilenceRequested()
  }

  override def onMessage(msg: Command): Behavior[Command] = msg match {
    case Signal(_, _)   => sendRequest(); this
    case SilenceRequest => sendRequest(); this
    case WakeRequest    => sendRequest(); this
    case Reset(replyTo) => reset(replyTo); this
    case other          => handleCommon(other); this
  }
}

object SilencingNeuronTyped {
  def apply(id: String,
            netId: String,
            synapses: List[SynapseTyped] = List.empty): Behavior[Command] =
    Behaviors.setup { context =>
      Behaviors.withTimers { timers =>
        NeuronCounter.reg(netId, id, context.self.toClassic)
        new SilencingNeuronTyped(context, timers, id, netId, synapses)
      }
    }
}
