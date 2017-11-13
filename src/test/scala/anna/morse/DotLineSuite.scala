package anna.morse

import anna.async.NetBuilder
import anna.blocks.{DelayGate, SignalSum}
import org.scalatest.junit.JUnitSuite
import anna.async.NetBuilderOps._
import org.junit.Assert._
import org.junit.Test

object DotLine {
  val in = "in"
  val dotOut = DelayGate.outputId("DOT")
  val lineOut = SignalSum.outputId("LINE")

  def builder: NetBuilder =
    NetBuilder().addInput(in)
      .delayGate("DOT", 2)
      .use(in).signalSum("LINE", 2)
      .use(dotOut).silence(SignalSum.silencingId("LINE"))
      .use(lineOut).silence(DelayGate.silencingId("DOT"))
}

class DotLineSuite extends JUnitSuite {
  import DotLine._

  @Test def shouldFireOnDot(): Unit = {
    val net = builder.build()

    var fired = false
    net.addAfterFire(dotOut){ fired = true }
    net.iterateUntilCalm("1,0,0,0")
    assertTrue(fired)

    net.shutdown()
  }

  @Test def shouldFireOnLine(): Unit = {
    val net = builder.build()

    var fired = false
    net.addAfterFire(lineOut){ fired = true }
    net.iterateUntilCalm("1,1,0,0")
    assertTrue(fired)

    net.shutdown()
  }

}
