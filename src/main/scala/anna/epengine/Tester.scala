package anna.epengine

import anna.async.NetBuilder
import anna.data.NetData
import anna.logger.LOG._

/**
 * Created by gorywoda on 05.01.15.
 */
class Tester(tests: List[NetTest]){
  def test(data: NetData):Double = {
    checkConditions(data)

    val (in, net) = NetBuilder().set(data).build("in")

    var result = 0.0
    tests.foreach( test => {
      net.reset()
      result += test.run(in, net)
    })

    net.shutdown()

    result
  }

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
