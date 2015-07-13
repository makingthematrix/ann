package anna.async

import scala.collection.mutable
import anna.logger.LOG._
import anna.logger.LOG
/**
 * Created by gorywoda on 23.06.15.
 */
object ActorCounter {
  LOG.addLogToFile("actorcounter.log")

  val nets = mutable.Set[String]()
  val neurons = mutable.Set[String]()

  def regNet(netId: String) = if(_enabled){
    //debug(s"REGISTER net $netId")
    if (nets.contains(netId)) exception(s"The net $netId is already registered")
    else nets += netId
  }

  def unregNet(netId: String) = if(_enabled){
    //debug(s"UNREGISTER net $netId")
    nets -= netId
  }

  def regNeuron(netId: String, neuronId: String) = if(_enabled){
    val _id = id(netId, neuronId)
    if (neurons.contains(_id)) exception(s"The neuron ${_id} is already registered")
    //debug(s"REGISTER neuron $neuronId in $netId")
    neurons += _id
  }

  def unregNeuron(netId: String, neuronId: String) = if(_enabled){
    //debug(s"UNREGISTER neuron $neuronId in $netId")
    neurons -= id(netId, neuronId)
  }

  def size = nets.size + neurons.size

  def clear() = {
    nets.clear()
    neurons.clear()
  }

  private var _enabled = true

  def enabled = _enabled
  def disable = { _enabled = false }
  def enable = { _enabled = true }

  private def id(netId: String, neuronId: String) = s"${netId}###${neuronId}"
}
