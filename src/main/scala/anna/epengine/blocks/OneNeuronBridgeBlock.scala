package anna.epengine.blocks

import anna.async.NetBuilder
import anna.data.{NetData, SynapseData, SynapseTrait, NeuronData}
import scala.collection.mutable
import anna.async.NetBuilderOps._

/**
 * Created by gorywoda on 10/23/15.
 */
class OneNeuronBridgeBlock(_neurons: mutable.ListBuffer[NeuronData],
                           _synapses: mutable.Map[String, SynapseData],
                           inIds: mutable.Set[String],
                           outIds: mutable.Set[String]) extends BuildingBlock {
  override def neurons: List[NeuronData] = _neurons.toList
  override def synapses: Map[String, SynapseData] = _synapses.toMap

  override def ins: List[NeuronData] = _neurons.filter(n => inIds.contains(n.id)).toList
  override def outs: List[NeuronData] = _neurons.filter(n => outIds.contains(n.id)).toList

  override def netData: NetData = ???

  override def changeNeuronId(currentId: String, newId: String): Unit = ???

  override def setWeight(fromId: String, toId: String, weight: SynapseTrait): Unit = ???
}

object OneNeuronBridgeBlock {
  def apply():OneNeuronBridgeBlock = {
    val builder = NetBuilder()
    builder.addInput("in1").chain("default",1.0,0.0).chain("out1",1.0,0.0)
    val data = builder.data

    val n = mutable.ListBuffer[NeuronData]()
    n ++= data.neurons // it would be nice to have "neuronsWithoutSynapses" here

    val s = mutable.HashMap[String, SynapseData]()
    s ++= data.synapses
    new OneNeuronBridgeBlock(n, s, mutable.Set("in1"), mutable.Set("out1"))
  }
}