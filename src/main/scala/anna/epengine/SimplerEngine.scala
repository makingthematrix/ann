package anna.epengine

import anna.Context
import anna.data.NetData
/**
  * Created by gorywoda on 1/8/16.
  */
class SimplerEngine(override val name: String,
                    override val coach: Coach,
                    override val mutator: Mutator,
                    protected val initialPoll: GenomePoll)
  extends StandardEngine(name, coach, mutator, SimplerCrosser.ID, initialPoll)

object SimplerEngine {
  def apply(coach: Coach, poll: GenomePoll, mutationsProfile: MutationsProfile):SimplerEngine =
    new SimplerEngine ("engine", coach, Mutator(mutationsProfile), poll)

  def apply(exercises: List[Exercise], genomes: List[NetGenome], mutationsProfile: MutationsProfile):SimplerEngine =
    new SimplerEngine("engine", Coach(exercises), Mutator(mutationsProfile), GenomePoll(genomes))

  def apply(name: String,
            inputIds: List[String],
            outputIds: List[String],
            netTemplate: NetData,
            exercisesSet: ExercisesSet,
            mutationsProfile: MutationsProfile): SimplerEngine = {
    val coach = Coach(exercisesSet)
    val poll = GenomePoll(netTemplate, inputIds, outputIds, Context().genomePollSize, mutationsProfile)
    new SimplerEngine(name, coach, Mutator(mutationsProfile), poll)
  }
}