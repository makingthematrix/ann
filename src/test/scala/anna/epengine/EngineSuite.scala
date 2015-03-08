package test.async.epengine

import anna.async.{NetRef, NetWrapper, NetBuilder}
import anna.data.NetData
import anna.epengine._
import anna.logger.LOG
import anna.logger.LOG._
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import anna.utils.Utils
import anna.async.NetBuilderOps._
import anna.epengine.MutationAccess._

class EngineSuite extends JUnitSuite {
  @Before def before() {
    LOG.addLogToStdout()
  }

  val inputIds = List("in1")
  val outputIds = List("out1")

  val anySignalAnyResponse = (wrapper: NetWrapper, good: Double, bad: Double) => {
    var counter = 0
    wrapper.addAfterFire("out1"){ counter += 1 }

    wrapper += "1"

    wrapper.tickUntilCalm()
    if(counter > 0) good else bad
  }

  val t1 = NetTest("any response to any signal", 1, List("out1"), anySignalAnyResponse)

  val f = (wrapper: NetWrapper, success: Double, failure: Double) => {
    var counter = 0
    wrapper.addAfterFire("out1"){ counter += 1 }

    wrapper += "1,1,1,1,1,1"

    wrapper.tickUntilCalm()
    if(counter == 6) success else failure
  }

  val t2 = NetTest("constant output", 1, List("out1"), f)

  @Test def shouldTestGenomePoll(): Unit ={
    val poll = GenomePoll("net", inputIds, outputIds, 5)
    assertEquals(5, poll.genomes.size)

    val results = Tester(List(t1)).test(poll)
    results.foreach( tuple => println(s"${tuple._1.id}: ${tuple._2}"))
  }

  @Test def shouldPerformEvolutionIteration(): Unit ={
    val poll = GenomePoll("net", inputIds, outputIds, 5)
    val tester = Tester(List(t1, t2))

    val engine = Engine(tester, 0.5, poll)
    val best1 = engine.best
    val result1 = tester.test(best1.data)

    engine.run()

    val best2 = engine.best
    val result2 = tester.test(best2.data)

    assertTrue(result2 >= result1)
  }
}