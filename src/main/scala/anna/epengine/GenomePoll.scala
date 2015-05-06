package anna.epengine

import anna.Context
import anna.data.NetData
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}
/**
 * Created by gorywoda on 13.02.15.
 */
case class GenomePoll(genomes: List[NetGenome]){
  def apply(id: String):NetGenome = genomes.find(_.id == id).get
  def apply(index: Int):NetGenome = genomes(index)
  def size = genomes.size
  def ids = genomes.map(_.id)

  def toJson = writePretty(this)
  
}

object GenomePoll {
  def apply(netId: String, inputIds: List[String], outputIds: List[String], size: Int):GenomePoll = {
    // the same assertions are in NetGenome.toss; I repeat them here so they won't appear for every net tossed.
    assert(Context().synapsesDensity >= 1.0, "There should be at least one synapse for neuron, is: " + Context().synapsesDensity)
    assert(inputIds.size + outputIds.size <= Context().neuronsRange.end, s"You chose ${inputIds.size} inputs and ${outputIds.size} outputs, but the max possible neurons number is only ${Context().neuronsRange.end}")
    new GenomePoll( (1 to size).map( i => NetGenome.build(netId + i, inputIds, outputIds) ).toList )
  }

  def apply(template: NetData, inputIds: List[String], outputIds: List[String], size: Int):GenomePoll = {
    assert(Context().synapsesDensity >= 1.0, "There should be at least one synapse for neuron, is: " + Context().synapsesDensity)
    assert(inputIds.size + outputIds.size <= Context().neuronsRange.end, s"You chose ${inputIds.size} inputs and ${outputIds.size} outputs, but the max possible neurons number is only ${Context().neuronsRange.end}")

    val accessMap = NetGenome.accessMap(inputIds, outputIds)
    new GenomePoll( 
      (1 to size).map( 
        i => NetGenome(template, accessMap).netId(template.id+i).mutate(Context().initialMutationsNumber)
      ).toList
    )
  }

  def fromJson(jsonStr: String) = read[GenomePoll](jsonStr)
}