package anna.epengine

import anna.utils.RandomNumber

import scala.annotation.tailrec
import scala.collection.mutable

object GenomeOrigin extends Enumeration {
  type GenomeOrigin = Value
  val CROSSED, CLONED, BEST = Value
}

import GenomeOrigin._

/**
  * Created by gorywoda on 12/27/15.
  */
abstract class Crosser(val poll: GenomePoll, val results:Map[String, Double]) {
  def size = poll.size
  def newGeneration(iteration: Int =1):List[NetGenome]

  def cross(g1: NetGenome, g2: NetGenome):(NetGenome, NetGenome)

  protected val _crossed = mutable.Set[String]() // ids of genomes which were crossed during the last call to newGeneration
  def crossed = _crossed.toSet
  protected val _cloned = mutable.Set[String]() // ids of genomes which were cloned during the last call to newGeneration
  def cloned = _cloned.toSet

  // a standard (but not necessary) standard for net genome id is "ng${generation}#${index}#${previous_generation_info}"
  // where previous_generation_info is:
  // 1. if the genome was cloned: "${parent_index}Cloned"
  // 2. if the genome was crossed "${parent1_index}x${parent2_index}Crossed"
  // 3. if this is the best genome from the previous generation: its full name, together with the generation number
  def generateNewId(generation: Int, index: Int, origin: GenomeOrigin, ng1: NetGenome, ng2: NetGenome = null) = origin match {
    case BEST => ng1.id
    case CLONED => s"ng${generation}#${index}#${Crosser.retrieveIndex(ng1.id)}Cloned"
    case CROSSED =>
      s"ng${generation}#${index}#${Crosser.retrieveIndex(ng1.id)}x${Crosser.retrieveIndex(ng2.id)}Crossed"
  }

  protected def drawId = normalizedResults match {
    case None => results.toList(0)._1
    case Some(nr) => Crosser.getId(RandomNumber(), nr.toList.sortBy(-_._2))
  }

  protected def drawGenome = poll(drawId)

  protected def normalizedResults = results.values.sum match {
    case 0.0 => None
    case sum =>
      val normalized = results.map(tuple => tuple._1 -> tuple._2 / sum)
      val z = normalized.values.min / 2.0
      val b = -z/(1.0-z)
      val a = 1.0-z
      Some(normalized.map(tuple => tuple._1 -> (a * tuple._2 + b)))
  }

  protected def drop(index: Int):List[NetGenome] = poll.genomes.take(index) ++ poll.genomes.drop(index + 1)
  protected def drop(id: String):List[NetGenome] = drop(poll.genomes.indexWhere(_.id == id))

  protected def drawCrossableGenomeFor(genome: NetGenome) = drawCrossableGenome(genome, drop(genome.id), results - genome.id)

  @tailrec
  private def drawCrossableGenome(firstGenome: NetGenome,
                                  genomes: List[NetGenome],
                                  results: Map[String, Double]):Option[NetGenome] = genomes match {
    case Nil => None
    case list =>
      val id = drawId
      val index = poll.genomes.indexWhere(_.id == id)
      val genome = poll.genomes(index)
      if(firstGenome.id != genome.id && firstGenome.crossable(genome)) Some(genome)
      else drawCrossableGenome(firstGenome, genomes.drop(index), results - id)
  }
}

trait CrosserBuilder {
  def build(poll: GenomePoll, results: Map[String, Double]):Crosser
}

object Crosser {
  private val crosserBuilders = mutable.Map[String, CrosserBuilder]()

  def apply(crosserId: String, poll: GenomePoll, results: Map[String, Double]) = crosserBuilders.get(crosserId) match {
    case Some(builder) => builder.build(poll, results)
    case None => throw new IllegalArgumentException(s"There is no registered crosser builder with id $crosserId")
  }

  def register(crosserId: String, builder: CrosserBuilder) = crosserBuilders.get(crosserId) match {
    case Some(builder) =>  throw new IllegalArgumentException(s"There is an already registered crosser builder with id $crosserId")
    case None => crosserBuilders += (crosserId -> builder)
  }

  @tailrec
  def getId(r: Double, resultsNormalized: List[(String, Double)]):String = resultsNormalized match {
    case head :: Nil => head._1
    case head :: tail if r <= head._2 => head._1
    case head :: tail => getId(r - head._2, tail)
  }

  val indexRetrievalPattern = "ng\\d+\\#(\\d+).*".r

  def retrieveIndex(netGenomeId: String) = netGenomeId match {
    case indexRetrievalPattern(index) => index
    case _ => netGenomeId
  }
}
