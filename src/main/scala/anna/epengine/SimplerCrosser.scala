package anna.epengine

import anna.logger.LOG._

/**
  * Created by gorywoda on 12/27/15.
  */
class SimplerCrosser(override val poll: GenomePoll, override val results:Map[String, Double]) extends Crosser(poll, results){
  // so far, only cloning is implemented. but fear not.
  override def newGeneration(iteration: Int): List[NetGenome] = {
    val best :: sortedGenomes = poll.genomesSorted(results)
    val clonedGenomes = for(i <- 0 until sortedGenomes.size) yield clone(sortedGenomes(i), s"iter${iteration}#${i + 1}Cloned")
    List(best) ++ clonedGenomes
  }

  private def clone(genome: NetGenome, newNetId: String) = {
    debug(this,s"CLONING: genome ${genome.id} as $newNetId")
    genome.netId(newNetId)
  }
}


object SimplerCrosser {
  def apply(poll: GenomePoll, results: Map[String,Double]) = new SimplerCrosser(poll, results)

  val ID = "SimplerCrosser"

  Crosser.register(ID, new CrosserBuilder {
    override def build(poll: GenomePoll, results: Map[String, Double]) = new SimplerCrosser(poll, results)
  })
}