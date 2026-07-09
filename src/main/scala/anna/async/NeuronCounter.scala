package anna.async

import java.util.concurrent.ConcurrentHashMap

import akka.actor.{ActorRef, PoisonPill}
import anna.logger.LOG
import anna.logger.LOG._

import scala.jdk.CollectionConverters._

/**
 * A process-local, thread-safe registry mapping a neuron's `netId###neuronId`
 * to its (classic) `ActorRef`.
 *
 * Historically this was backed by a classic `akka.actor.TypedActor` purely to
 * serialize access to a mutable map. `TypedActor` (the "Active Object" pattern)
 * was deprecated in Akka 2.6 and dropped in 2.9, and it is the wrong tool here:
 * this is not message-driven behaviour, just shared state with synchronous
 * accessors. It is therefore backed by a [[ConcurrentHashMap]] instead — no
 * actor, no blocking asks, same API, and thread-safe by construction.
 */
trait NeuronCounter {
  def reg(netId: String, neuronId: String, ref: ActorRef): Unit
  def unreg(netId: String, neuronId: String): Unit
  def neurons: Map[String, ActorRef]
  def size: Int
  def clean(): Unit
}

class NeuronCounterImpl(val name: String) extends NeuronCounter {
  def this() = this("default")

  LOG.addLogToFile("neuroncounter.log")

  private val _neurons = new ConcurrentHashMap[String, ActorRef]()

  def reg(netId: String, neuronId: String, ref: ActorRef): Unit = {
    val _id = NeuronCounter.id(netId, neuronId)
    // putIfAbsent makes the "check-then-insert" atomic. On a duplicate we log an
    // error rather than throwing: the old TypedActor dispatched reg as a one-way
    // message, so a duplicate never propagated an exception back to the caller.
    if (_neurons.putIfAbsent(_id, ref) != null)
      error(this, s"The neuron ${_id} is already registered")
  }

  def unreg(netId: String, neuronId: String): Unit = {
    _neurons.remove(NeuronCounter.id(netId, neuronId))
  }

  def neurons: Map[String, ActorRef] = _neurons.asScala.toMap

  def size: Int = _neurons.size

  def clean(): Unit = if (!_neurons.isEmpty) {
    val str = _neurons.keySet.asScala.mkString(",")
    error(this, s"${_neurons.size} needs cleaning, which means something went wrong... $str")
    _neurons.values.asScala.foreach(_ ! PoisonPill)
    _neurons.clear()
  }
}

object NeuronCounter {
  private var instanceOpt: Option[NeuronCounter] = None

  def set(nc: NeuronCounter): Unit = synchronized {
    stop()
    instanceOpt = Some(nc)
  }

  def apply(): NeuronCounter = synchronized {
    instanceOpt match {
      case Some(instance) => instance
      case None =>
        val instance = new NeuronCounterImpl("default")
        instanceOpt = Some(instance)
        instance
    }
  }

  def reg(netId: String, neuronId: String, ref: ActorRef): Unit = if(_enabled) apply().reg(netId, neuronId, ref)

  def unreg(netId: String, neuronId: String): Unit = if(_enabled) apply().unreg(netId, neuronId)

  def neurons: Map[String, ActorRef] = if(_enabled) apply().neurons else Map[String, ActorRef]()

  def size: Int = if(_enabled) apply().size else -1

  def clean(): Unit = if(_enabled) apply().clean()

  private def stop(): Unit = synchronized {
    if(_enabled) instanceOpt.foreach { instance =>
      instance.clean()
      instanceOpt = None
    }
  }

  private var _enabled = true

  def enabled: Boolean = _enabled
  def disable(): Unit = { _enabled = false }
  def enable(): Unit = { _enabled = true }

  def id(netId: String, neuronId: String) = s"${netId}###${neuronId}"
}
