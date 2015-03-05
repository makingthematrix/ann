package anna.epengine

import anna.utils.RandomNumber

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

import anna.logger.LOG._

class Engine(val tester: Tester,
             val mutationProbability: Probability,
             private var poll: GenomePoll,
             private var results:Map[String,Double] = HashMap()) {
  assert(poll.genomes.size >= 2, "There have to be at least two genomes in the engine's poll")
  private var initialized = false

  def run() = iteration(1)
  
  def iteration(iterIndex: Int) = {
    debug(this,s"iteration $iterIndex")
    if(!initialized){
      debug(this,"not initialized yet so calculating results for the same time")
      calculateResults()
      initialized = true
    }
    debug(this,s"results: $results")
    val sum = results.values.sum
    debug(this,s"sum: $sum")
    val resultsNormalized = results.map( tuple => (tuple._1 -> tuple._2/sum) ).toList.sortBy(-_._2).toMap
    debug(this,s"results normalized: $resultsNormalized")
    // single out one genome from the lower half and cross it
    val lowerHalfGenome = poll(resultsNormalized.toList(RandomNumber(results.size/2) + results.size/2)._1)
    debug(this,s"lower half genome id: ${lowerHalfGenome.id}")
    val higherHalfGenomeOpt = Engine.drawCrossableGenome(lowerHalfGenome, poll.genomes, results)
    if(higherHalfGenomeOpt != None) debug(this,s"higher half genome id: ${higherHalfGenomeOpt.get.id}")
    else debug(this, s"higher half genome: None")
    // this is to ensure that the new poll will be at least a bit diversified
    val (g1,g2) = if(higherHalfGenomeOpt != None) lowerHalfGenome.cross(higherHalfGenomeOpt.get)
                  else (lowerHalfGenome.clone, lowerHalfGenome.clone)
    debug(this, s"new genomes: ${g1.id} and ${g2.id}")
    val newGenomes = (1 to results.size/2 - 1).map( i => {
      debug(this,s"$i")
      val genome1 = poll(drawId)
      val genome2Opt = Engine.drawCrossableGenome(genome1, drop(genome1.id), results - genome1.id)
      val (g1, g2) = if(genome2Opt != null) genome1.cross(genome2Opt.get) else (genome1.clone, genome1.clone)
      g1.setNetId(s"iter${iterIndex}Left")
      g2.setNetId(s"iter${iterIndex}Right")
      List(g1, g2)
    }).flatten.toList ++ List(g1, g2)
    debug(this, "the whole list of new genomes: " + newGenomes.map(_.id))
    newGenomes.foreach( g => if(mutationProbability.toss) g.mutate())
    poll = GenomePoll(newGenomes)
    calculateResults()
  }

  def best = poll.genomes.find( _.id == results.maxBy(_._2)._1 ).get

  private def calculateResults(): Unit ={
    debug(this,"calculateResults")
    results = tester.test(poll).map( tuple => (tuple._1.id -> tuple._2) ).toMap
  }

  private def drawId = Engine.drawId(results)
  private def drop(id: String) = Engine.drop(poll.genomes, id)
}

object Engine {
  def apply(tester: Tester, mutationProbability: Probability, poll: GenomePoll) =
    new Engine(tester, mutationProbability, poll, poll.genomes.map(g => (g.id -> 0.0)).toMap)

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
    else getId(RandomNumber(), results.map(tuple => (tuple._1 -> tuple._2 / sum)).toList.sortBy(-_._2))
  }

  @tailrec
  private def getId(r: Double, resultsNormalized: List[(String, Double)]):String = resultsNormalized match {
    case head :: Nil => head._1
    case head :: tail if r <= head._2 => head._1
    case head :: tail => getId(r - head._2, tail)
  }
}