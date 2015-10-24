package anna.epengine.blocks

import anna.data.{SynapseTrait, NetData, SynapseData, NeuronData}

import scala.collection.mutable
import anna.utils.Utils.{synapseId, fromId, toId}

/**
 * Created by gorywoda on 10/24/15.
 */
class StandardBuildingBlock( override val name: String,
                             _neurons: mutable.ListBuffer[NeuronData],
                            _synapses: mutable.Map[String, SynapseData],
                            inIds: mutable.Set[String],
                            outIds: mutable.Set[String]) extends BuildingBlock {
  override def neurons: List[NeuronData] = _neurons.toList
  override def synapses: Map[String, SynapseData] = _synapses.toMap

  override def ins: List[NeuronData] = _neurons.filter(n => inIds.contains(n.id)).toList
  override def phantomOuts: List[NeuronData] = _neurons.filter(n => outIds.contains(n.id)).toList

  override def changeNeuronId(currentId: String, newId: String): Unit = _neurons.find(n => n.id == currentId) match {
    case Some(n) =>
      _neurons -= n
      _neurons += n.withId(newId)
      // the order is important: we have to first replace ids in "toSynapses" because they require us to call .withId
      // so, in case we have a neuron with a synapse to itself, we have to change synapse.neuronId
      // if we replaced ids first in "fromSynapses", the key would be changed twice in replaceAll and we would not find
      // this synapse again when looking for toSynapses, so we would not call its newId. Got it? :)
      val toSynapses = _synapses.filterKeys(_.contains(toId(currentId)))
      _synapses --= toSynapses.keys
      _synapses ++= toSynapses.map( tuple => tuple._1.replaceAll(currentId, newId) -> tuple._2.withId(newId))

      val fromSynapses = _synapses.filterKeys(_.contains(fromId(currentId)))
      _synapses --= fromSynapses.keys
      _synapses ++= fromSynapses.map( tuple => tuple._1.replaceAll(currentId, newId) -> tuple._2 )
    case None => throw new IllegalArgumentException(s"No neuron found with id $currentId")
  }

  override def setWeight(fromId: String, toId: String, weight: SynapseTrait): Unit = {
    val sId = synapseId(fromId, toId)
    _synapses.get(sId) match {
      case Some(s) =>
        _synapses -= sId
        _synapses += (sId -> s.withWeight(weight))
      case None => throw new IllegalArgumentException(s"No synapse found $sId")
    }
  }

  override def netData: NetData = {
    val foo = if(removePhantomOuts) neurons.filterNot(n => outIds.contains(n.id)) else neurons
    val neuronsList = foo.map( n => n.withSynapses(synapses.filterKeys(_.contains(fromId(n.id))).values.toList)).sortBy( _.id )
    NetData(name, neuronsList, inIds.toList.sorted)
  }
}

object StandardBuildingBlock {
  def apply(name: String,
            neurons: Iterable[NeuronData],
            synapses: Map[String, SynapseData],
            inIds: Iterable[String],
            outIds: Iterable[String]) = {
    val n = mutable.ListBuffer[NeuronData]()
    n ++= neurons
    val s = mutable.Map[String, SynapseData]()
    s ++ synapses
    val i = mutable.Set[String]()
    i ++= inIds
    val o = mutable.Set[String]()
    o ++= outIds
    new StandardBuildingBlock(name, n, s, i, o)
  }
}