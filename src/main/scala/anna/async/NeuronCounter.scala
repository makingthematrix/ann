package anna.async

import anna.Context

import scala.collection.mutable
import anna.logger.LOG._
import anna.logger.LOG
import akka.actor.{ActorRef, ActorSystem, PoisonPill, TypedActor, TypedActorExtension, TypedProps}

/**
 * Created by gorywoda on 23.06.15.
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

  private val _neurons = mutable.Map[String, ActorRef]()

  def reg(netId: String, neuronId: String, ref: ActorRef):Unit = {
    val _id = NeuronCounter.id(netId, neuronId)
    if (_neurons.contains(_id)) exception(s"The neuron ${_id} is already registered")
    _neurons += _id -> ref
  }

  def unreg(netId: String, neuronId: String):Unit = {
    _neurons -= NeuronCounter.id(netId, neuronId)
  }

  def neurons:Map[String, ActorRef] = _neurons.toMap

  def size:Int = _neurons.size

  def clean():Unit = if(_neurons.nonEmpty){
    val str = _neurons.keys.mkString(",")
    error(this,s"${_neurons.size} needs cleaning, which means something went wrong... $str")
    _neurons.foreach(t => t._2 ! PoisonPill)
    _neurons.clear()
  }
}

object NeuronCounter {
  private var instanceOpt: Option[NeuronCounter] = None

  def set(nc: NeuronCounter): Unit = {
    stop()
    instanceOpt = Some(nc)
  }

  def apply(): NeuronCounter = instanceOpt match {
    case Some(instance) => instance
    case None =>
      val typedProps: TypedProps[NeuronCounter] = TypedProps(classOf[NeuronCounter], new NeuronCounterImpl("default"))
      val typedActor: TypedActorExtension = TypedActor(Context().system)
      val instance: NeuronCounter = typedActor.typedActorOf(typedProps)
      instanceOpt = Some(instance)
      instance
  }

  def reg(netId: String, neuronId: String, ref: ActorRef): Unit = if(_enabled) apply().reg(netId, neuronId, ref)

  def unreg(netId: String, neuronId: String): Unit = if(_enabled) apply().unreg(netId, neuronId)

  def neurons: Map[String, ActorRef] = if(_enabled) apply().neurons else Map[String, ActorRef]()

  def size: Int = if(_enabled) apply().size else -1

  def clean(): Unit = if(_enabled) apply().clean()

  private def stop(): Unit = if(_enabled && instanceOpt.isDefined){
    instanceOpt.get.clean()
    TypedActor(Context().system).poisonPill(instanceOpt.get)
    instanceOpt = None
  }

  private var _enabled = true

  def enabled: Boolean = _enabled
  def disable(): Unit = { _enabled = false }
  def enable(): Unit = { _enabled = true }

  def id(netId: String, neuronId: String) = s"${netId}###${neuronId}"
}
