package anna.epengine

import anna.Context
import anna.logger.LOG._
import anna.utils.RandomNumber

/**
  * Created by gorywoda on 12/26/15.
  */
class StandardCrosser(override val poll: GenomePoll, override val results:Map[String, Double]) extends Crosser(poll, results) {

  def newGeneration(iteration: Int):List[NetGenome] = {
    debug(this, " --- new generation --- ")
    debug(this,s"Poll size ${poll.size}, results: ${results.size}")

    val genomesToCross = math.round(Context().crossCoefficient * (results.size-1)).toInt
    val crossedGenomes = crossRandomGenomes(iteration, genomesToCross)
    val genomesToClone = results.size - crossedGenomes.size
    val clonedGenomes = cloneGenomes(iteration, genomesToClone)

    debug(this,s"genomesToCross: ${crossedGenomes.size}, genomesToClone: ${clonedGenomes.size}")
    debug(this, s" --- new generation done --- ")

    crossedGenomes ++ clonedGenomes
  }

  private def cloneGenomes(iteration: Int, size: Int):List[NetGenome] = {
    val sortedGenomes = poll.genomesSorted(results)
    val bestGenome = sortedGenomes(0)
    val lowerHalfRandom = RandomNumber((sortedGenomes.size / 2) until sortedGenomes.size)
    val lowerHalfGenome = sortedGenomes(lowerHalfRandom).netId(s"iter${iteration}#${lowerHalfRandom}Cloned")

    List(bestGenome, lowerHalfGenome) ++ (2 until size).map(index => {
      val genome = sortedGenomes(index - 1)
      val newId = s"iter${iteration}#${index - 1}Cloned"
      debug(this,s"CLONING: genome ${genome.id} as $newId")
      _cloned.add(genome.id)
      genome.netId(newId)
    })
  }

  private def crossRandomGenomes(iteration: Int, size: Int) = {
    val newGenomes = (for(i <- 1 to size/2) yield {
      val (g1, g2) = crossTwoGenomes(poll(drawId))
      debug(this,s"CROSSING: ... new names ${g1.id} -> iter${iteration}#${i}Left, ${g2.id} -> iter${iteration}#${i}Right")
      List(g1.netId(s"iter${iteration}#${i}Left"), g2.netId(s"iter${iteration}#${i}Right"))
    }).flatten.toList
    if(newGenomes.size > size) newGenomes.init else newGenomes
  }

  override def cross(g1: NetGenome, g2: NetGenome):(NetGenome, NetGenome) = {
    debug(this,s"CROSSING: ${g1.id} with ${g2.id}")
    _crossed.add(g1.id)
    _crossed.add(g2.id)
    g1.cross(g2)
  }

  private def crossTwoGenomes(genome1: NetGenome) = drawCrossableGenomeFor(genome1) match {
    case Some(genome2) => cross(genome1, genome2)
    case None =>
      debug(this,s"CROSSING: ${genome1.id} in two copies as no other crossable genome was found")
      _cloned.add(genome1.id)
      (genome1.clone, genome1.clone)
  }

}

object StandardCrosser {
  def apply(poll: GenomePoll, results: Map[String,Double]) = new StandardCrosser(poll, results)

  val ID = "StandardCrosser"

  Crosser.register(ID, new CrosserBuilder {
    override def build(poll: GenomePoll, results: Map[String, Double]) = new StandardCrosser(poll, results)
  })
}
