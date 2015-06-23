package anna.async

import scala.collection.mutable
import anna.logger.LOG.exception
/**
 * Created by gorywoda on 23.06.15.
 */
object ActorCounter {
  val nets = mutable.Set[String]()
  val neurons = mutable.Set[String]()

  def regNet(id: String) = if(nets.contains(id)) exception(s"The net $id is already registered") else nets += id
  def unregNet(id: String) = nets -= id
  def regNeuron(id: String) = if(neurons.contains(id)) exception(s"The neuron $id is already registered") else neurons += id
  def unregNeuron(id: String) = neurons -= id

  def size = nets.size + neurons.size

  def clear() = {
    nets.clear()
    neurons.clear()
  }
}
