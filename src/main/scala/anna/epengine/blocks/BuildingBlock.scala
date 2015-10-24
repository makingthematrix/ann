package anna.epengine.blocks

import anna.data.{NetData, SynapseTrait, SynapseData, NeuronData}
import anna.utils.Utils.synapseId

/**
 * Created by gorywoda on 10/23/15.
 */
trait BuildingBlock {
  val name: String

  var removePhantomOuts = true

  def neurons:List[NeuronData]
  def ins:List[NeuronData]
  def phantomOuts:List[NeuronData]
  def synapses:Map[String,SynapseData] // the key is synapseId(fromId,toId)

  def netData: NetData

  def setWeight(fromId: String, toId: String, weight: SynapseTrait)
  def changeNeuronId(currentId: String, newId: String)

  def contains(neuronId: String):Boolean = neurons.exists(n => n.id == neuronId)
  def contains(fromId: String, toId: String):Boolean = synapses.contains(synapseId(fromId, toId))
  def weight(fromId: String, toId: String) = synapses(synapseId(fromId, toId)).weight
}

object BuildingBlock {
  def createBlock(blockName: String): BuildingBlock = blockName match {
    case "oneneuronbridge" => OneNeuronBridgeBlock()
  }

}