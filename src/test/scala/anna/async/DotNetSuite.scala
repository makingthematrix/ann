package anna.async

import anna.async.NetBuilderOps._
import anna.logger.LOG
import anna.logger.LOG.debug
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

class DotNetSuite extends JUnitSuite {
  val s = "1,0,0,0,1,0,0,0,1,0,0,0"
  val o = "1,1,0,0,1,1,0,0,1,1,0,0"

  private def dotNet3 = {
    NetBuilder().addInput("in")
    // dots
      .use("in").chain("mi11", 1.0, 0.0, 2).silence("mi11")
      .chain("mi12",1.0,0.0).loop("loop1",1.0,0.0,1.0)
      .chain("dot",0.6/2.0,0.6).silence("mi12").silence("loop1").silence("dot")
      .build()
  }

  @Test def shouldHaveDotInterval3() = {
    val netWrapper = dotNet3
    debug("------------")

    var dots = 0
    netWrapper += s

    LOG.timer()
    netWrapper.addAfterFire("in"){ debug("received input") }
    netWrapper.addAfterFire("dot"){ debug("Dot!"); dots += 1 }

    val interval = netWrapper.iterateUntilCalm()
    debug(s"interval: $interval, dots: $dots")
    assertEquals(3, dots)

    dots = 0
    netWrapper += o

    // without the other part of the network which would recognize the "line" signals,
    // lines should also be recognized as dots - maybe the additional '1's mean only that the signal is noised

    val interval2 = netWrapper.iterateUntilCalm()
    debug(s"interval: $interval, dots: $dots")
    assertEquals(3, dots)

    netWrapper.shutdown()
  }
}