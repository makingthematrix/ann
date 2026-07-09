package anna.async

import anna.async.NetBuilderOps._
import anna.logger.LOG.debug
import org.junit.Assert._
import org.scalatest.funsuite.AnyFunSuite

/**
 * Line-recognizer network tests, built with [[NetBuilder]] + [[NetBuilderOps]]
 * and driven through [[NetWrapper]].
 */
class LineNetSuite extends AnyFunSuite {
  val s = "1,0,0,0,1,0,0,0,1,0,0,0"
  val o = "1,1,0,0,1,1,0,0,1,1,0,0"

  private def lineNet2: (NetWrapper, StringBuilder) = {
    val netWrapper = NetBuilder()
           .addInput("in1")
           .chain("mi21", 0.4, 0.6).silence("mi21")
           .chain("out2", 1.0)
           .build()
    debug("----------")
    val sb = new StringBuilder()
    netWrapper.addAfterFire("out2") {
      debug("Line!")
      sb.append('-')
    }

    (netWrapper, sb)
  }

  test("shouldLineThenNothing1") {
    val (netWrapper, sb) = lineNet2

    netWrapper += "1,1,0,0,0,0"

    netWrapper.iterateUntilCalm()
    assertEquals("-", sb.toString)

    netWrapper.shutdown()
  }

  test("shouldLine3Times") {
    val (netWrapper, sb) = lineNet2

    netWrapper += "1,1,0,1,1,0,1,1,0"

    netWrapper.iterateUntilCalm()
    assertEquals("---", sb.toString)

    netWrapper.shutdown()
  }

  private def lineNet3 =
    NetBuilder().addInput("in")
           .chain("mi21", 0.5, 0.55)
           .silence("mi21")
           .chain("line", 1.0, 0.0).silence("line")
           .build()

  test("shouldHaveLineInterval3") {
    val netWrapper = lineNet3
    debug("------------")
    var lines = 0
    netWrapper.addAfterFire("in") { debug("Incoming!") }
    netWrapper.addAfterFire("line") { debug("Line!"); lines += 1; }

    netWrapper += o

    val interval = netWrapper.iterateUntilCalm()
    debug(s"interval: $interval, lines: $lines")
    assertEquals(3, lines)

    lines = 0
    netWrapper += s
    netWrapper.iterateUntilCalm()
    debug(s"lines: $lines")
    assertEquals(1, lines)
    // Without the other part of the network recognizing 'dots', two consecutive '1's, even with an interval
    // between them, should be recognized as a line. The interval may mean that the signal is noised.

    netWrapper.shutdown()
  }
}
