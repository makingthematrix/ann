package anna.epengine

import anna.Context
import org.json4s.JsonAST.{JString, JArray, JField}
import org.json4s.native.JsonMethods._

/**
 * Created by gorywoda on 13.02.15.
 */
class GenomePoll(val genomes: List[NetGenome]){
  def apply(id: String) = genomes.find(_.id == id).get
  def size = genomes.size

  def toPrettyJson = {
    val genomesJson = genomes.map{ g => JString(g.data.toPrettyJson) }
    pretty(render(JArray(genomesJson)))
  }

  def toJson = {
    val genomesJson = genomes.map{ g => JString(g.data.toJson) }
    compact(render(JArray(genomesJson)))
  }
}

object GenomePoll {
  def apply(genomes: List[NetGenome]):GenomePoll = new GenomePoll(genomes)
  def apply(netId: String, inputIds: List[String], outputIds: List[String], size: Int):GenomePoll = {
    // the same assertions are in NetGenome.toss; I repeat them here so they won't appear for every net tossed.
    assert(Context().synapsesDensity >= 1.0, "There should be at least one synapse for neuron")
    assert(inputIds.size + outputIds.size <= Context().neuronsRange.end, s"You chose ${inputIds.size} inputs and ${outputIds.size} outputs, but the max possible neurons number is only ${Context().neuronsRange.end}")
    new GenomePoll( (1 to size).map( i => NetGenome.toss(netId + i, inputIds, outputIds) ).toList )
  }
}