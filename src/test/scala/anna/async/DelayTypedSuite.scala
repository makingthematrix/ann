package anna.async

import anna.Context
import anna.async.NetBuilderOpsTyped._
import anna.logger.LOG
import org.junit.Assert._
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

/**
 * Typed-Actor counterpart of [[DelaySuite]].
 *
 * Each test mirrors the same-named test in `DelaySuite`, driving the typed
 * stack via [[NetBuilderTyped]] + [[NetBuilderOpsTyped]] and
 * [[NetWrapperTyped]].
 */
class DelayTypedSuite extends AnyFunSuite {
  test("foo") {
    val netWrapper = NetBuilderTyped().addInput("in1").build("net")

    netWrapper += "1"

    LOG.timer()
    val list = mutable.ListBuffer[Long]()
    netWrapper.addAfterFire("in1", "fired") { list += LOG.time }

    netWrapper.iterateUntilCalm()

    netWrapper.shutdown()
  }

  test("shouldGiveConstantOutput") {
    val netWrapper = NetBuilderTyped().addInput("in1").chain("mi1", 1.0).chain("out1", 1.0, 0.75).build("net")

    netWrapper += "1,1,1,1,1,1"

    LOG.timer()
    val list = mutable.ListBuffer[Long]()
    netWrapper.addAfterFire("out1", "fired") { list += LOG.time }

    netWrapper.iterateUntilCalm()

    val tolerance = Context().iterationTime
    //assertEqualsWithTolerance(produceSeq(6, tolerance, Context().iterationTime), list, tolerance)

    netWrapper.shutdown()
  }

  test("shouldCreateOscillatorWithMethod2") {
    val netWrapper = NetBuilderTyped().addInput("in").chain("mi1", 1.0).oscillator().chain("out1", 1.0, 0.75).build()

    netWrapper += "1,1,1,1,1,1"

    val list = mutable.ListBuffer[Int]()
    netWrapper.addAfterFire("out1", "fired") {
      list += netWrapper.iteration
    }

    netWrapper.iterateUntilCalm()

    assertEquals(List(1, 3, 5), list.toList)

    netWrapper.shutdown()
  }

  test("shouldCreateOscillator2") {
    val netWrapper = NetBuilderTyped().addInput("in1").chain("mi1", 1.0).loop("osc", 1.0, 0.5, -1.0).chain("out1", 1.0, 0.75)
                                      .use("osc").chain("mi2", 1.0).chain("out2", 1.0, 0.75)
                                      .build()

    netWrapper += "1,1,1,1,1,1"

    val sb = new StringBuilder()
    netWrapper.addAfterFire("out1", "fired 1") { sb.append('1') }
    netWrapper.addAfterFire("out1", "fired 0") { sb.append('0') }

    netWrapper.iterateUntilCalm()

    assertEquals("101010", sb.toString)

    netWrapper.shutdown()
  }
}
