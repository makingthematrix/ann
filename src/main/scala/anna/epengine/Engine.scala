package anna.epengine

import anna.utils.RandomNumber

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

import anna.logger.LOG._

class Engine(val coach: Coach,
             val mutationProbability: Probability,
             private var _poll: GenomePoll,
             private var results:Map[String,Double] = HashMap()) {
  import Engine._

  assert(_poll.genomes.size >= 2, "There have to be at least two genomes in the engine's poll")
  private var iterIndex = 0

  def poll = _poll

  def run(iterations: Int =1) = {
    val end = iterIndex + iterations
    while(iterIndex < end) iteration()
  }
  
  def iteration() = {
    if(iterIndex == 0) calculateResults()

    iterIndex += 1

    debug(this,s" ------------------------ Iteration $iterIndex of the engine ------------------------ ")

    _poll = GenomePoll(mutate(newGeneration))

    calculateResults()

    debug(this,s" ------------------------ done iteration $iterIndex of the engine ------------------------ ")
  }

  def mutate(genomes: List[NetGenome]) = {
    debug(this, s" --- mutating --- ")
    genomes.foreach( g => if(mutationProbability.toss) {
      g.mutate()
      g.data.validate()
    })
    debug(this, s" --- done mutating --- ")
    genomes
  }

  def newGeneration = {
    debug(this, " --- new generation --- ")
    debug(this,s"Poll size ${_poll.size}, results: ${results.size}")

    val resultsNormalized = {
      val sum = results.values.sum
      results.map( tuple => tuple._1 -> tuple._2/sum ).toList.sortBy(-_._2).toMap
    }

    // single out one genome from the lower half and cross it
    // this is to ensure that the new poll will be at least a bit diversified
    val lowerHalfGenome = _poll(resultsNormalized.toList(RandomNumber(results.size/2) + results.size/2)._1)
    val higherHalfGenomeOpt = drawCrossableGenome(lowerHalfGenome, _poll.genomes, results)
    val (g1,g2) = if(higherHalfGenomeOpt != None) lowerHalfGenome.cross(higherHalfGenomeOpt.get)
                  else (lowerHalfGenome.clone, lowerHalfGenome.clone)
    debug(this,s"new names: ${g1.id} -> iter${iterIndex}#0Left, ${g2.id} -> iter${iterIndex}#0Right")

    val newGenomes = (for(i <- 1 to results.size/2 - 1) yield {
      val (g1, g2) = crossTwoGenomes
      debug(this,s"new names: ${g1.id} -> iter${iterIndex}#${i}Left, ${g2.id} -> iter${iterIndex}#${i}Right")
      List(g1.netId(s"iter${iterIndex}#${i}Left"), g2.netId(s"iter${iterIndex}#${i}Right"))
    }).flatten.toList ++ List(g1.netId(s"iter${iterIndex}#0Left"), g2.netId(s"iter${iterIndex}#0Right"))

    if(newGenomes.size < _poll.size) best.netId(s"iter${iterIndex}Best") :: newGenomes else newGenomes
  }

  def crossTwoGenomes = {
    val genome1 = _poll(drawId)
    drawCrossableGenome(genome1, drop(genome1.id), results - genome1.id) match {
      case Some(genome2) => genome1.cross(genome2)
      case None => (genome1.clone, genome1.clone)
    }
  }

  def best = _poll.genomes.find( _.id == results.maxBy(_._2)._1 ).get

  def getResult(netId: String) = results.get(netId)

  def calculateResults(): Unit ={
    debug(this,"------------------------------ calculate results ------------------------------")
    debug(this,s"There are ${poll.size} genomes in the poll")
    debug(this,poll.ids.toString())
    results = coach.test(_poll).map( tuple => tuple._1.id -> tuple._2 ).toMap
    debug(this,s"And there ${results.size} results")
    if(iterIndex == 0) iterIndex = 1
    debug(this,"------------------------------ done calculating results ------------------------------")
  }

  private def drawId = Engine.drawId(results)
  private def drop(id: String) = Engine.drop(_poll.genomes, id)
}

object Engine {
  def apply(tester: Coach, mutationProbability: Probability, poll: GenomePoll) =
    new Engine(tester, mutationProbability, poll, poll.genomes.map(g => g.id -> 0.0).toMap)

  @tailrec
  private def drawCrossableGenome(firstGenome: NetGenome,
                                  genomes: List[NetGenome],
                                  results: Map[String, Double]):Option[NetGenome] = genomes match {
    case Nil => None
    case list =>
      debug(this,s"drawCrossableGenome(${firstGenome.id},${genomes.size},${results.size})")
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