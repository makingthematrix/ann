package anna.epengine

import anna.async.{NeuronCounter, NetBuilder}
import anna.data.NetData
import anna.logger.LOG
import anna.logger.LOG._

/**
 * Created by gorywoda on 05.01.15.
 */
class Coach(val exercises: List[Exercise]){
  def                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             test(data: NetData):Double = {
    debug(this, s" -------------- testing ${data.id} ------------------")
    checkConditions(data)

    var result = 0.0
    val wrapper = NetBuilder().set(data).build()
    exercises.foreach( ex => {
      LOG.resetTimer()
      wrapper.removeAllTriggers()

      LOG.debug(this, s"running ${ex.name}")

      val t = ex.run(wrapper)

      LOG.timer(this, s"running ${ex.name} finished with result $t")

      result += t
    })
    wrapper.shutdown()


    debug(this, s" -------------- done testing ${data.id} with result $result ------------------")
    math.max(result, 0.0)
  }

  def test(poll: GenomePoll):List[(NetGenome,Double)] = poll.genomes.map(genome => (genome, test(genome.data))).sortBy(-_._2).toList

  private def checkConditions(data: NetData): Unit ={
    exercises.foreach( ex => {
      if (ex.inputLen != data.inputs.size) exception(this, s"${ex.name}: ${ex.inputLen} input neurons required, but ${data.id} contains ${data.inputs.size}")
      val ids = data.neurons.map(_.id).toSet
      ex.outputIds.foreach(id => {
        if (!ids.contains(id)) exception(this, s"${ex.name}: ${data.id} does not contain required neuron ${id}")
      })
    })
  }
}

object Coach {
  def apply(exercises: List[Exercise]):Coach = new Coach(exercises)
  def apply(exercisesSet: ExercisesSet):Coach = {
    exercisesSet.validate
    apply(exercisesSet.exercises)
  }
}
