package anna.epengine

import anna.Context
import anna.data.NetData
import anna.logger.{ListLogOutput, LOG}
import anna.logger.LOG._
import anna.utils.{RandomNumber, Utils}

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

class Engine(val dirName: String,
             val coach: Coach,
             val savingProgress: Boolean,
             private var _poll: GenomePoll,
             private var results:Map[String,Double] = HashMap()) {
  import anna.epengine.Engine._

  assert(_poll.genomes.size >= 2, "There have to be at least two genomes in the engine's poll")
  private var iterIndex = 0

  def poll = _poll

  def run(iterations: Int =1) = {
    val end = iterIndex + iterations
    while(iterIndex < end) iteration()
  }
  
  def iteration() = {
    if(iterIndex == 0) calculateResults()

    val listOut = new ListLogOutput(s"iteration${iterIndex}")
    LOG.addOut(listOut)

    debug(this,s" ------------------------ Iteration $iterIndex of the engine ------------------------ ")

    _poll = GenomePoll(mutate(newGeneration))

    if(savingProgress) Utils.save(s"${dirPath}/poll_iteration${iterIndex}.json", _poll.toJson)

    calculateResults()

    debug(this,s" ------------------------ done iteration $iterIndex of the engine ------------------------ ")

    Utils.save(s"${dirPath}/iteration${iterIndex}.log", listOut.log)
    val mutations = listOut.list.filter(_.contains("MUTATION: ")).map(l => l.substring(l.indexOf("MUTATION: ") + 10))
    Utils.save(s"${dirPath}/mutations_iteration${iterIndex}.log", mutations.mkString("\n"))
    LOG.removeOut(listOut)

    Utils.save(s"${dirPath}/best_iteration${iterIndex}.json", best.toJson)

    iterIndex += 1
  }

  def mutate(genomes: List[NetGenome]) = {
    debug(this, s" --- mutating --- ")
    genomes.foreach( g => if(Probability(Context().mutationProbability).toss) {
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

    if(savingProgress) Utils.save(s"${dirPath}/results_iteration${iterIndex}.json", writePretty(results))

    if(iterIndex == 0) iterIndex = 1
    debug(this,"------------------------------ done calculating results ------------------------------")
  }

  def dirPath = Context().evolutionDir + "/" + dirName

  private def drawId = Engine.drawId(results)
  private def drop(id: String) = Engine.drop(_poll.genomes, id)
}

object Engine {
  def apply(coach: Coach, poll: GenomePoll):Engine =
    new Engine("engine", coach, false, poll, initResultsMap(poll))

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
    new Engine(dirName, coach, true, poll, initResultsMap(poll))
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
    val engine = new Engine(dirName, coach, true, poll, initResultsMap(poll))
    engine.iterIndex = findGenomePollIteration(dirPath)
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

  private def initResultsMap(poll: GenomePoll) = poll.genomes.map(g => g.id -> 0.0).toMap
}