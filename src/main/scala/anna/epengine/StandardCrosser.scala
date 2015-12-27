package anna.epengine

import anna.Context
import anna.logger.LOG._
import anna.utils.RandomNumber

import scala.annotation.tailrec

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

  private def crossTwoGenomes(genome1: NetGenome) = drawCrossableGenome(genome1, drop(genome1.id), results - genome1.id) match {
    case Some(genome2) =>
      debug(this,s"CROSSING: ${genome1.id} with ${genome2.id}")
      genome1.cross(genome2)
    case None =>
      debug(this,s"CROSSING: ${genome1.id} in two copies as no other crossable genome was found")
      (genome1.clone, genome1.clone)
  }

  private def drawId = normalizedResults match {
    case None => results.toList(0)._1
    case Some(nr) => StandardCrosser.getId(RandomNumber(), nr.toList.sortBy(-_._2))
  }

  private def normalizedResults = results.values.sum match {
    case 0.0 => None
    case sum =>
      val normalized = results.map(tuple => tuple._1 -> tuple._2 / sum)
      val z = normalized.values.min / 2.0
      val b = -z/(1.0-z)
      val a = 1.0-z
      Some(normalized.map(tuple => tuple._1 -> (a * tuple._2 + b)))
  }

  private def drop(index: Int):List[NetGenome] = poll.genomes.take(index) ++ poll.genomes.drop(index + 1)
  private def drop(id: String):List[NetGenome] = drop(poll.genomes.indexWhere(_.id == id))

  @tailrec
  private def drawCrossableGenome(firstGenome: NetGenome,
                                  genomes: List[NetGenome],
                                  results: Map[String, Double]):Option[NetGenome] = genomes match {
    case Nil => None
    case list =>
      debug(this,s"drawCrossableGenome(${firstGenome.id},${poll.genomes.size},${results.size})")
      val id = drawId
      val index = poll.genomes.indexWhere(_.id == id)
      val genome = poll.genomes(index)
      if(firstGenome.id != genome.id && firstGenome.crossable(genome)) Some(genome)
      else drawCrossableGenome(firstGenome, genomes.drop(index), results - id)
  }

}

object StandardCrosser {
  @tailrec
  def getId(r: Double, resultsNormalized: List[(String, Double)]):String = resultsNormalized match {
    case head :: Nil => head._1
    case head :: tail if r <= head._2 => head._1
    case head :: tail => getId(r - head._2, tail)
  }

  val ID = "StandardCrosser"

  Crosser.register(ID, new CrosserBuilder {
    override def build(poll: GenomePoll, results: Map[String, Double]) = new StandardCrosser(poll, results)
  })
}
