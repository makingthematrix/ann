package anna.epengine

import anna.Context
import anna.data.NetData
import anna.logger.LOG._
import anna.logger.{LOG, ListLogOutput}
import anna.utils.{RandomNumber, Utils}
import scala.annotation.tailrec
import scala.collection.mutable

class StandardEngine(override val name: String,
                     override val coach: Coach,
                     override protected var _poll: GenomePoll,
                     val mutationsProfile: MutationsProfile) extends EngineLike {
  import anna.epengine.StandardEngine._
  assert(_poll.genomes.size >= 2, "There have to be at least two genomes in the engine's poll")

  override protected def _run() = {
    if(_iteration == 0) calculateResults()

    val listOut = new ListLogOutput(s"iteration${_iteration}")
    LOG.addOut(listOut)

    debug(this,s" ------------------------ Iteration ${_iteration} of the standard engine $name ------------------------ ")

    _poll = GenomePoll(mutate(newGeneration))

    calculateResults()

    debug(this,s" ------------------------ done iteration ${_iteration} of the standard engine $name ------------------------ ")

    val mutations = listOut.list.map(_ match {
      case line if line.contains("MUTATION: ") => Some(line.substring(line.indexOf("MUTATION: ")))
      case line if line.contains("CROSSING: ") => Some(line.substring(line.indexOf("CROSSING: ")))
      case line if line.contains("CLONING: ") => Some(line.substring(line.indexOf("CLONING: ")))
      case _ => None
    }).flatten

    LOG.removeOut(listOut)

    _iteration += 1
  }

  def mutate(genomes: List[NetGenome]) = {
    debug(this, s" --- mutating --- ")
    // we don't mutate the best one
    genomes.filterNot(_.id == newBestId).foreach( g => if(Probability(Context().mutationProbability).toss) {
      mutationsProfile.mutate(g, RandomNumber(Context().mutationsPerGenome))
      g.data.validate()
    })
    debug(this, s" --- done mutating --- ")
    genomes
  }

  def newGeneration:List[NetGenome] = {
    debug(this, " --- new generation --- ")
    debug(this,s"Poll size ${_poll.size}, results: ${_results.size}")

    val genomesToCross = math.round(Context().crossCoefficient * (_results.size-1)).toInt
    val crossedGenomes = crossRandomGenomes(genomesToCross)
    val genomesToClone = _results.size - crossedGenomes.size
    val clonedGenomes = cloneGenomes(genomesToClone)

    debug(this,s"genomesToCross: ${crossedGenomes.size}, genomesToClone: ${clonedGenomes.size}")
    debug(this, s" --- new generation done --- ")

    crossedGenomes ++ clonedGenomes
  }

  private def newBestId = s"iter${_iteration}#0Cloned-Best"

  private def cloneGenomes(size: Int) = if(size > 0){
    val sortedGenomes = _poll.genomesSorted(_results)
    val bestGenome = sortedGenomes(0)

    debug(this,s"CLONING: best genome ${bestGenome.id} as $newBestId")

    val list = mutable.ListBuffer[NetGenome](bestGenome.netId(newBestId))
    if(size > 1){
      val lowerHalfRandom = RandomNumber((sortedGenomes.size / 2) until sortedGenomes.size)
      val lowerHalfGenome = sortedGenomes(lowerHalfRandom)
      val newId = s"iter${_iteration}#${lowerHalfRandom}Cloned"
      debug(this,s"CLONING: lower half ($lowerHalfRandom) genome ${lowerHalfGenome.id} as $newId")
      list += lowerHalfGenome.netId(newId)
      list ++= (2 until size).map(index => {
        val genome = sortedGenomes(index - 1)
        val newId = s"iter${_iteration}#${index - 1}Cloned"
        debug(this,s"CLONING: genome ${genome.id} as $newId")
        genome.netId(newId)
      })
    }
    list.toList
  } else List[NetGenome]()

  private def crossRandomGenomes(size: Int) = {
    val newGenomes = (for(i <- 1 to size/2) yield {
      val (g1, g2) = crossTwoGenomes
      debug(this,s"CROSSING: ... new names ${g1.id} -> iter${_iteration}#${i}Left, ${g2.id} -> iter${_iteration}#${i}Right")
      List(g1.netId(s"iter${_iteration}#${i}Left"), g2.netId(s"iter${_iteration}#${i}Right"))
    }).flatten.toList
    if(newGenomes.size > size) newGenomes.init else newGenomes
  }

  private def crossTwoGenomes = {
    val genome1 = _poll(drawId)
    drawCrossableGenome(genome1, drop(genome1.id), _results - genome1.id) match {
      case Some(genome2) =>
        debug(this,s"CROSSING: ${genome1.id} with ${genome2.id}")
        genome1.cross(genome2)
      case None =>
        debug(this,s"CROSSING: ${genome1.id} in two copies as no other crossable genome was found")
        (genome1.clone, genome1.clone)
    }
  }

  private def drawId = StandardEngine.drawId(_results)
  private def drop(id: String) = StandardEngine.drop(_poll.genomes, id)
}

object StandardEngine {
  def apply(coach: Coach, poll: GenomePoll, mutationsProfile: MutationsProfile):StandardEngine = {
    new StandardEngine ("engine", coach, poll, mutationsProfile)
  }

  def apply(name: String,
            inputIds: List[String],
            outputIds: List[String],
            netTemplate: NetData,
            exercisesSet: ExercisesSet,
            mutationsProfile: MutationsProfile): StandardEngine = {
    val coach = Coach(exercisesSet)
    val poll = GenomePoll(netTemplate, inputIds, outputIds, Context().genomePollSize, mutationsProfile)
    new StandardEngine(name, coach, poll, mutationsProfile)
  }

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

  private def normalize(results: Map[String, Double]) = results.values.sum match {
    case 0.0 => None
    case sum =>
      val normalized = results.map(tuple => tuple._1 -> tuple._2 / sum)
      val z = normalized.values.min / 2.0
      val b = -z/(1.0-z)
      val a = 1.0-z
      Some(normalized.map(tuple => tuple._1 -> (a * tuple._2 + b)))
  }

  private def drawId(results: Map[String, Double]):String = normalize(results) match {
    case None => results.toList(0)._1
    case Some(normalizedResults) => getId(RandomNumber(), normalizedResults.toList.sortBy(-_._2))
  }

  @tailrec
  private def getId(r: Double, resultsNormalized: List[(String, Double)]):String = resultsNormalized match {
    case head :: Nil => head._1
    case head :: tail if r <= head._2 => head._1
    case head :: tail => getId(r - head._2, tail)
  }

}