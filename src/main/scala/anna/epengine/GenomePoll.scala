package anna.epengine

/**
 * Created by gorywoda on 13.02.15.
 */
class GenomePoll(val genomes: List[NetGenome]){

}

object GenomePoll {
  def apply(netId: String, inputIds: List[String], outputIds: List[String], size: Int):GenomePoll = {
    // the same assertions are in NetGenome.toss; I repeat them here so they won't appear for every net tossed.
    assert(NetGenome.synapsesDensity >= 1.0, "There should be at least one synapse for neuron")
    assert(inputIds.size + outputIds.size <= NetGenome.neuronsRange.end, s"You chose ${inputIds.size} inputs and ${outputIds.size} outputs, but the max possible neurons number is only ${NetGenome.neuronsRange.end}")
    new GenomePoll( (1 to size).map( i => NetGenome.toss(netId + i, inputIds, outputIds) ).toList )
  }
}