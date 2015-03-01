package anna.epengine

import anna.async.NetBuilder
import anna.data.NetData
import anna.logger.LOG._

/**
 * Created by gorywoda on 05.01.15.
 */
class Tester(tests: List[NetTest]){
  def test(data: NetData):Double = {
    debug(this, s"testing ${data.id}")
    checkConditions(data)

    val wrapper = NetBuilder().set(data).build("in")

    var result = 0.0
    tests.foreach( test => {
      wrapper.removeAllTriggers()
      wrapper.reset()
      result += test.run(wrapper)
    })

    wrapper.shutdown()

    debug(this, s"the result is $result")
    result
  }

  def test(poll: GenomePoll):List[(NetGenome, Double)] = poll.genomes.map( genome => (genome, test(genome.data)) ).sortBy(-_._2)

  private def checkConditions(data: NetData): Unit ={
    tests.foreach( test => {
      if (test.inputLen != data.inputs.size) exception(this, s"${test.name}: ${test.inputLen} input neurons required, but ${data.id} contains ${data.inputs.size}")
      val ids = data.neurons.map(_.id).toSet
      test.outputIds.foreach(id => {
        if (!ids.contains(id)) exception(this, s"${test.name}: ${data.id} does not contain required neuron ${id}")
      })
    })
  }
}

object Tester {
  def apply(tests: List[NetTest]) = new Tester(tests)
}
