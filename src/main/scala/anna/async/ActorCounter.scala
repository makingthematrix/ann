package anna.async

import scala.collection.mutable
import anna.logger.LOG.exception
/**
 * Created by gorywoda on 23.06.15.
 */
object ActorCounter {
  val nets = mutable.Set[String]()
  val neurons = mutable.Set[String]()

  def regNet(netId: String) =
    if(nets.contains(netId)) exception(s"The net $netId is already registered")
    else nets += netId
  def unregNet(netId: String) = nets -= netId
  def regNeuron(netId: String, neuronId: String) = {
    val _id = id(netId, neuronId)
    if (neurons.contains(_id)) exception(s"The neuron ${_id} is already registered")
    else neurons += _id
  }
  def unregNeuron(netId: String, neuronId: String) = neurons -= id(netId, neuronId)

  def size = nets.size + neurons.size

  def clear() = {
    nets.clear()
    neurons.clear()
  }

  private def id(netId: String, neuronId: String) = s"${netId}###${neuronId}"
}
