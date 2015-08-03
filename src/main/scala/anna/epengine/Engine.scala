package anna.epengine

import anna.Context
import anna.data.NetData
import anna.logger.LOG._
import anna.logger.{LOG, ListLogOutput}
import anna.utils.Utils.formats
import anna.utils.{RandomNumber, Utils, IntRange}
import org.json4s.native.Serialization.{read, writePretty}

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable

class Engine(val dirName: String, val coach: Coach, private var _poll: GenomePoll) extends EngineLike {
  import anna.epengine.Engine._
  assert(_poll.genomes.size >= 2, "There have to be at least two genomes in the engine's poll")

  def poll = _poll

  def calculateResults(): Unit ={
    debug(this,"------------------------------ calculate results ------------------------------")
    debug(this,s"There are ${poll.size} genomes in the poll")
    debug(this,poll.ids.toString())
    _results = coach.test(_poll).map( tuple => tuple._1.id -> tuple._2 ).toMap
    debug(this,s"And there ${_results.size} results")

    Utils.save(s"${dirPath}/results_iteration${_iteration}.json", writePretty(_results))

    if(_iteration == 0) _iteration = 1
    debug(this,"------------------------------ done calculating results ------------------------------")
  }

  protected def _run() = {
    if(_iteration == 0) calculateResults()

    val listOut = new ListLogOutput(s"iteration${_iteration}")
    LOG.addOut(listOut)

    debug(this,s" ------------------------ Iteration ${_iteration} of the engine ------------------------ ")

    _poll = GenomePoll(mutate(newGeneration))

    Utils.save(s"${dirPath}/poll_iteration${_iteration}.json", _poll.toJson)

    calculateResults()

    debug(this,s" ------------------------ done iteration ${_iteration} of the engine ------------------------ ")

    Utils.save(s"${dirPath}/iteration${_iteration}.log", listOut.log)

    val mutations = listOut.list.map(_ match {
      case line if line.contains("MUTATION: ") => Some(line.substring(line.indexOf("MUTATION: ")))
      case line if line.contains("CROSSING: ") => Some(line.substring(line.indexOf("CROSSING: ")))
      case line if line.contains("CLONING: ") => Some(line.substring(line.indexOf("CLONING: ")))
      case _ => None
    }).flatten

    Utils.save(s"${dirPath}/mutations_iteration${_iteration}.log", mutations.mkString("\n"))
    Utils.save(s"${dirPath}/best_iteration${_iteration}.json", best.toJson)

    LOG.removeOut(listOut)

    _iteration += 1
  }

  def mutate(genomes: List[NetGenome]) = {
    debug(this, s" --- mutating --- ")
    // we don't mutate the best one
    genomes.filterNot(_.id == newBestId).foreach( g => if(Probability(Context().mutationProbability).toss) {
      g.mutate(RandomNumber(Context().mutationsPerGenome))
      g.data.validate()
    })
    debug(this, s" --- done mutating --- ")
    genomes
  }

  def newGeneration:List[NetGenome] = {
    debug(this, " --- new generation --- ")
    debug(this,s"Poll size ${_poll.size}, results: ${_results.size}")

    val genomesToCross = math.round(Context().crossCoefficient * _results.size).toInt
    val genomesToClone = _results.size - genomesToCross

    debug(this,s"genomesToCross: $genomesToCross, genomesToClone: $genomesToClone")

    val crossedGenomes = crossRandomGenomes(genomesToCross)
    val clonedGenomes = cloneGenomes(genomesToClone)

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

  def dirPath = Context().evolutionDir + "/" + dirName

  private def drawId = Engine.drawId(_results)
  private def drop(id: String) = Engine.drop(_poll.genomes, id)
}

object Engine {
  def apply(coach: Coach, poll: GenomePoll):Engine =
    new Engine("engine", coach, poll)

  def apply(dirName: String, inputIds: List[String], outputIds: List[String], netTemplate: NetData, exercisesSet: ExercisesSet): Engine = {
    val dirPath = Context().evolutionDir + "/" + dirName
    Utils.createDir(dirPath)
    Utils.save(dirPath + "/inputIds.json", writePretty(inputIds))
    Utils.save(dirPath + "/outputIds.json", writePretty(outputIds))
    Utils.save(dirPath + "/context.json", Context().toJson)
    Utils.save(dirPath + "/netTemplate.json", netTemplate.toJson)
    Utils.save(dirPath + "/exercisesSet.json", exercisesSet.toJson)

    val coach = Coach(exercisesSet)
    val poll = GenomePoll(netTemplate, inputIds, outputIds, Context().genomePollSize)
    Utils.save(dirPath + "/poll_iteration0.json", poll.toJson)
    new Engine(dirName, coach, poll)
  }

  def apply(dirName: String):Engine = {
    val dirPath = Context().evolutionDir + "/" + dirName
    if(!Utils.fileExists(dirPath)) exception(s"There is no directory $dirPath")

    // this one has to be there
    val exercisesSet = ExercisesSet.fromJson(Utils.load(dirPath + "/exercisesSet.json"))
    // if there is no context we can simply use the current one
    if(Utils.fileExists(dirPath + "/context.json")) Context.withJson(Utils.load(dirPath + "/context.json"))
    // if there is no poll we can attempt to create it
    val poll = loadPoll(dirPath) match {
      case Some(poll) => poll
      case None =>
        val inputIds = read[List[String]](Utils.load(dirPath + "/inputIds.json"))
        val outputIds = read[List[String]](Utils.load(dirPath + "/outputIds.json"))
        val netTemplate = NetData.fromJson(Utils.load(dirPath + "/netTemplate.json"))
        GenomePoll(netTemplate, inputIds, outputIds, Context().genomePollSize)
    }

    val coach = Coach(exercisesSet)
    val engine = new Engine(dirName, coach, poll)
    engine._iteration = findGenomePollIteration(dirPath)
    engine
  }

  private def findGenomePollIteration(dirPath: String) = {
    var counter = -1
    while(Utils.fileExists(s"${dirPath}/poll_iteration${counter+1}.json")) counter += 1
    counter
  }

  private def loadPoll(dirPath: String):Option[GenomePoll] = {
    val iteration = findGenomePollIteration(dirPath)
    if(iteration >= 0) Some(GenomePoll.fromJson(Utils.load(s"${dirPath}/poll_iteration${iteration}.json")))
    else None
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