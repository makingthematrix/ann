package anna.async

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import anna.async.Neuron._
import anna.data.Wake

/**
 * A [[Neuron]] with threshold `0.0` and no silencing of its own that, instead
 * of accumulating and firing, turns any incoming signal or silence/wake request
 * into Silence/Wake requests forwarded to its synapse destinations.
 *
 * The queries and triggers of the base neuron (`GetData`, `GetSynapses`, the
 * trigger messages, `Reset`, ...) keep working through the inherited
 * `handleCommon` / `reset`.
 */
final class SilencingNeuron(context: ActorContext[Command],
                                 timers: TimerScheduler[Command],
                                 override val id: String,
                                 override val netId: String,
                                 initialSynapses: List[Synapse])
  extends Neuron(context, timers, id, netId, 0.0, 0, initialSynapses) {

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

object SilencingNeuron {
  def apply(id: String,
            netId: String,
            synapses: List[Synapse] = List.empty): Behavior[Command] =
    Behaviors.setup { context =>
      Behaviors.withTimers { timers =>
        NeuronCounter.reg(netId, id, context.self.toClassic)
        new SilencingNeuron(context, timers, id, netId, synapses)
      }
    }
}
