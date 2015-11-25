package anna.epengine.blocks

import anna.data.{SynapseTrait, NetData, SynapseData, NeuronData}

import scala.collection.mutable
import anna.utils.Utils.{synapseId, fromId, toId}
import anna.logger.LOG._

/**
 * Created by gorywoda on 10/24/15.
 */
class StandardBuildingBlock( override val name: String,
                             private var _neurons: List[NeuronData],
                             private var _synapses: Map[String, SynapseData],
                             private var inIds: Set[String],
                             private var outIds: Set[String]) extends BuildingBlock {
  override def neurons: List[NeuronData] = _neurons
  override def synapses: Map[String, SynapseData] = _synapses

  override def ins: List[NeuronData] = _neurons.filter(n => inIds.contains(n.id)).toList
  override def phantomOuts: List[NeuronData] = _neurons.filter(n => outIds.contains(n.id)).toList

  override def changeNeuronId(currentId: String, newId: String): Unit =
    if(_neurons.exists(n => n.id == currentId)) {
      _neurons = _neurons.map(n => if (n.id == currentId) n.withId(newId) else n)
      _synapses = _synapses.keys.map(oldKey => (oldKey.replaceAll(currentId, newId) -> _synapses(oldKey))).toMap
    } else throw new IllegalArgumentException(s"No neuron found with id $currentId")

  override def setWeight(fromId: String, toId: String, weight: SynapseTrait): Unit = {
    val sId = synapseId(fromId, toId)
    _synapses.get(sId) match {
      case Some(s) =>
        _synapses = _synapses.filterNot( tuple => tuple._1 == sId ) + (sId -> s.withWeight(weight))
      case None => throw new IllegalArgumentException(s"No synapse found with id $sId")
    }
  }

  override def netData: NetData = {
    debug(this, s"netData, neurons: ${_neurons.map(_.id)}, synapses: ${_synapses.keys}")
    val ns = if(removePhantomOuts) {
      _neurons.filterNot(n => outIds.contains(n.id))
    } else _neurons
    val neuronsList = ns.map( n => {
      val fId = fromId(n.id)
      debug(this, s"fromId: $fId")
      val sn = _synapses.filterKeys(_.contains(fId)).values.toList
      debug(this, s"sn: $sn")
      val nn = n.withSynapses(sn)
      debug(this, s"${n.id}: ${n.synapses.map(_.neuronId)}")
      nn
    }).sortBy( _.id )
    NetData(name, neuronsList, inIds.toList.sorted)
  }
}

object StandardBuildingBlock {
  def apply(name: String,
            neurons: Iterable[NeuronData],
            synapses: Map[String, SynapseData],
            inIds: Iterable[String],
            outIds: Iterable[String]) = {
    new StandardBuildingBlock(name, neurons.toList, synapses, inIds.toSet, outIds.toSet)
  }
}