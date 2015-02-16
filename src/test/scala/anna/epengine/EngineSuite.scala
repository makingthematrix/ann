package test.async.epengine

import anna.async.{NetRef, NetInput, NetBuilder}
import anna.epengine._
import anna.logger.LOG
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite
import org.junit.Assert._

class EngineSuite extends JUnitSuite {
  @Before def before() {
    LOG.addLogToStdout()

    LOG.track(Tester.getClass())
    LOG.trackAll = false
  }

  val inputIds = List("in1")
  val outputIds = List("out1")

  val anySignalAnyResponse = (ni: NetInput, ref: NetRef, good: Double, bad: Double) => {
    var counter = 0
    ref.addAfterFire("out1"){ counter += 1 }

    ni += "1"

    ni.tickUntilCalm()
    if(counter > 0) good else bad
  }

  val t1 = NetTest("any response to any signal", 1, List("out1"), anySignalAnyResponse)

  @Test def shouldTestGenomePoll(): Unit ={
    val poll = GenomePoll("net", inputIds, outputIds, 10)
    assertEquals(10, poll.genomes.size)

    val results = Tester(List(t1)).test(poll)
    results.foreach( tuple => println(s"${tuple._1.id}: ${tuple._2}"))
  }
}