package anna.async

import anna.Context

import scala.collection.mutable
import anna.logger.LOG._
import anna.logger.LOG

import akka.actor.{PoisonPill, ActorRef, TypedProps, TypedActor}

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
    debug(s"REGISTER neuron $neuronId in $netId")
    _neurons += _id -> ref
  }

  def unreg(netId: String, neuronId: String):Unit = {
    debug(s"UNREGISTER neuron $neuronId in $netId")
    _neurons -= NeuronCounter.id(netId, neuronId)
  }

  def neurons:Map[String, ActorRef] = _neurons.toMap

  def size:Int = _neurons.size

  def clean():Unit = if(_neurons.nonEmpty){
    val str = _neurons.map(_._1).mkString(",")
    debug(s"${_neurons.size} needs cleaning, which means something went wrong... $str")
    _neurons.foreach(t => t._2 ! PoisonPill)
    _neurons.clear()
  }
}

object NeuronCounter {
  private var instanceOpt: Option[NeuronCounter] = None

  def set(nc: NeuronCounter) = {
    stop()
    instanceOpt = Some(nc)
  }

  def apply() = instanceOpt match {
    case Some(instance) => instance
    case None =>
      val instance = TypedActor(Context().system).typedActorOf(TypedProps(classOf[NeuronCounter], new NeuronCounterImpl("default")), "name")
      instanceOpt = Some(instance)
      instance
  }

  def reg(netId: String, neuronId: String, ref: ActorRef) = if(_enabled) apply().reg(netId, neuronId, ref)

  def unreg(netId: String, neuronId: String) = if(_enabled) apply().unreg(netId, neuronId)

  def neurons = if(_enabled) apply().neurons else Map[String, ActorRef]()

  def size = if(_enabled) apply().size else -1

  def clean() = if(_enabled) apply().clean()

  def stop() = if(_enabled && instanceOpt != None){
    clean()
    TypedActor(Context().system).poisonPill(instanceOpt.get)
    instanceOpt = None
  }

  private var _enabled = true

  def enabled = _enabled
  def disable = { _enabled = false }
  def enable = { _enabled = true }

  def id(netId: String, neuronId: String) = s"${netId}###${neuronId}"
}
