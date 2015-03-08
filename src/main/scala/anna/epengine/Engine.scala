package anna.epengine

import anna.utils.RandomNumber

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

class Engine(val tester: Tester,
             val mutationProbability: Probability,
             private var poll: GenomePoll,
             private var results:Map[String,Double] = HashMap()) {
  import Engine._

  assert(poll.genomes.size >= 2, "There have to be at least two genomes in the engine's poll")
  private var iterIndex = 0

  def run(iterations: Int =1) = {
    val end = iterIndex + iterations
    while(iterIndex < end) iteration()
  }
  
  def iteration() = {
    if(iterIndex == 0) calculateResults()

    iterIndex += 1

    poll = GenomePoll(mutate(newGeneration))

    calculateResults()
  }

  def mutate(genomes: List[NetGenome]) = {
    genomes.foreach( g => if(mutationProbability.toss) g.mutate())
    genomes
  }

  def newGeneration = {
    val resultsNormalized = {
      val sum = results.values.sum
      results.map( tuple => tuple._1 -> tuple._2/sum ).toList.sortBy(-_._2).toMap
    }

    // single out one genome from the lower half and cross it
    // this is to ensure that the new poll will be at least a bit diversified
    val lowerHalfGenome = poll(resultsNormalized.toList(RandomNumber(results.size/2) + results.size/2)._1)
    val higherHalfGenomeOpt = drawCrossableGenome(lowerHalfGenome, poll.genomes, results)
    val (g1,g2) = if(higherHalfGenomeOpt != None) lowerHalfGenome.cross(higherHalfGenomeOpt.get)
                  else (lowerHalfGenome.clone, lowerHalfGenome.clone)

    val newGenomes = (1 to results.size/2 - 1).map( i => {
      val genome1 = poll(drawId)
      val genome2Opt = drawCrossableGenome(genome1, drop(genome1.id), results - genome1.id)
      val (g1, g2) = if(genome2Opt != null) genome1.cross(genome2Opt.get)
                     else (genome1.clone, genome1.clone)
      List(g1.netId(s"iter${iterIndex}Left"), g2.netId(s"iter${iterIndex}Right"))
    }).flatten.toList ++ List(g1, g2)

    if(newGenomes.size < poll.size) best :: newGenomes else newGenomes
  }

  def best = poll.genomes.find( _.id == results.maxBy(_._2)._1 ).get

  private def calculateResults(): Unit ={
    results = tester.test(poll).map( tuple => tuple._1.id -> tuple._2 ).toMap
  }

  private def drawId = Engine.drawId(results)
  private def drop(id: String) = Engine.drop(poll.genomes, id)
}

object Engine {
  def apply(tester: Tester, mutationProbability: Probability, poll: GenomePoll) =
    new Engine(tester, mutationProbability, poll, poll.genomes.map(g => g.id -> 0.0).toMap)

  @tailrec
  private def drawCrossableGenome(firstGenome: NetGenome,
                                  genomes: List[NetGenome],
                                  results: Map[String, Double]):Option[NetGenome] = genomes match {
    case Nil => None
    case list =>
      val id = drawId(results)
      val index = genomes.indexWhere(_.id == id)
      val genome = genomes(index)
      if(firstGenome.id != genome.id && firstGenome.crossable(genome)) Some(genome)
      else drawCrossableGenome(firstGenome, drop(genomes, index), results - id)
  }

  private def drop(genomes: List[NetGenome], index: Int):List[NetGenome] = genomes.take(index) ++ genomes.drop(index + 1)
  private def drop(genomes: List[NetGenome], id: String):List[NetGenome] = drop(genomes, genomes.indexWhere(_.id == id))

  private def drawId(results: Map[String, Double]):String = {
    val sum = results.values.sum
    if(sum == 0.0) results.toList(0)._1
    else getId(RandomNumber(), results.map(tuple => tuple._1 -> tuple._2 / sum).toList.sortBy(-_._2))
  }

  @tailrec
  private def getId(r: Double, resultsNormalized: List[(String, Double)]):String = resultsNormalized match {
    case head :: Nil => head._1
    case head :: tail if r <= head._2 => head._1
    case head :: tail => getId(r - head._2, tail)
  }
}