package anna.epengine

import anna.Context
import anna.data.NetData
import anna.logger.LOG._
import anna.logger.{LOG, ListLogOutput}
import anna.utils.{Utils}
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}

/**
 * Created by gorywoda on 06.08.15.
 */
class MonteCarloEngine(override val name: String,
                       override val coach: Coach,
                       override protected var _poll: GenomePoll,
                       val netTemplate: NetData,
                       val mutationsPerNewGenome: Int) extends EngineLike {

  override protected def _run(): Unit = {
    if(_iteration == 0) calculateResults()

    val listOut = new ListLogOutput(s"iteration${_iteration}")
    LOG.addOut(listOut)

    debug(this,s" ------------------------ Iteration ${_iteration} of the standard engine $name ------------------------ ")

    _poll = GenomePoll(newGeneration)

    Utils.save(s"${dirPath}/poll_iteration${_iteration}.json", _poll.toJson)

    calculateResults()

    debug(this,s" ------------------------ done iteration ${_iteration} of the standard engine $name ------------------------ ")

    Utils.save(s"${dirPath}/iteration${_iteration}.log", listOut.log)

    val mutations = listOut.list.map(_ match {
      case line if line.contains("MUTATION: ") => Some(line.substring(line.indexOf("MUTATION: ")))
      case line if line.contains("CLONING: ") => Some(line.substring(line.indexOf("CLONING: ")))
      case _ => None
    }).flatten

    Utils.save(s"${dirPath}/mutations_iteration${_iteration}.log", mutations.mkString("\n"))
    Utils.save(s"${dirPath}/best_iteration${_iteration}.json", best.toJson)

    LOG.removeOut(listOut)

    _iteration += 1
  }

  def newGeneration:List[NetGenome] = {
    // i don't like it we have to take the access map from the best genome instead of some higher level place
    // after all, this is the same access map for all genomes
    val bestGenome = best
    val accessMap = bestGenome.accessMap
    debug(this,s"CLONING: best genome ${bestGenome.id} as $newBestId")
    bestGenome.netId(newBestId) :: GenomePoll.newGeneration(bestGenome.data, accessMap, _poll.size - 1, mutationsPerNewGenome)
  }

  private def newBestId = s"iter${_iteration}#0"
}

object MonteCarloEngine {
  def apply(name: String,
            inputIds: List[String],
            outputIds: List[String], 
            netTemplate: NetData,
            exercisesSet: ExercisesSet,
            mutationsPerNewGenome: Int = Context().initialMutationsNumber): MonteCarloEngine = {
    val dirPath = Context().evolutionDir + "/" + name
    Utils.createDir(dirPath)
    Utils.save(dirPath + "/inputIds.json", writePretty(inputIds))
    Utils.save(dirPath + "/outputIds.json", writePretty(outputIds))
    Utils.save(dirPath + "/context.json", Context().toJson)
    Utils.save(dirPath + "/netTemplate.json", netTemplate.toJson)
    Utils.save(dirPath + "/exercisesSet.json", exercisesSet.toJson)

    val coach = Coach(exercisesSet)
    val poll = GenomePoll(netTemplate, inputIds, outputIds, Context().genomePollSize, mutationsPerNewGenome)
    Utils.save(dirPath + "/poll_iteration0.json", poll.toJson)
    new MonteCarloEngine(name, coach, poll, netTemplate, mutationsPerNewGenome)
  }
  
  
}
