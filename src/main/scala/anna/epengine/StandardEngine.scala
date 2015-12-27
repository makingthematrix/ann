package anna.epengine

import anna.Context
import anna.data.NetData
import anna.logger.LOG._
import anna.logger.{LOG, ListLogOutput}

class StandardEngine(override val name: String,
                     override val coach: Coach,
                     val mutator: Mutator,
                     val crosserId: String,
                     override protected var _poll: GenomePoll) extends Engine {
  assert(_poll.genomes.size >= 2, "There have to be at least two genomes in the engine's poll")

  override protected def _run() = {
    if(_iteration == 0) calculateResults()

    val listOut = new ListLogOutput(s"iteration${_iteration}")
    LOG.addOut(listOut)

    debug(this,s" ------------------------ Iteration ${_iteration} of the standard engine $name ------------------------ ")

    val newGeneration = Crosser(crosserId, _poll, _results).newGeneration(_iteration)
    _poll = GenomePoll(mutator.mutate(newGeneration))

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
}

object StandardEngine {
  def apply(coach: Coach, poll: GenomePoll, mutationsProfile: MutationsProfile):StandardEngine = {
    new StandardEngine ("engine", coach, Mutator(mutationsProfile), StandardCrosser.ID, poll)
  }

  def apply(name: String,
            inputIds: List[String],
            outputIds: List[String],
            netTemplate: NetData,
            exercisesSet: ExercisesSet,
            mutationsProfile: MutationsProfile): StandardEngine = {
    val coach = Coach(exercisesSet)
    val poll = GenomePoll(netTemplate, inputIds, outputIds, Context().genomePollSize, mutationsProfile)
    new StandardEngine(name, coach, Mutator(mutationsProfile), StandardCrosser.ID, poll)
  }
}