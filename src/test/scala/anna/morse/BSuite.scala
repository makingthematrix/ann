package anna.morse

import anna.blocks.{Sequencer, SignalSum}
import org.scalatest.junit.JUnitSuite
import anna.async.NetBuilderOps._
import org.junit.Assert._
import org.junit.Test

class BSuite extends JUnitSuite {
  import DotLine._

  private val bOut = Sequencer.outputId("B")

  private def buildB() = builder.use(dotOut).signalSum("3DOTS", 3)
    .use(lineOut).silence(SignalSum.silencingId("3DOTS"))
    .sequencer("B", lineOut, SignalSum.outputId("3DOTS"))
    .build()

  @Test def shouldFireOnB(): Unit = {
    val net = buildB()

    var fired = false
    net.addAfterFire(bOut){ fired = true }
    net.iterateUntilCalm("1,1,0,0,1,0,0,0,1,0,0,0,1,0,0,0")
    assertTrue(fired)

    net.shutdown()
  }
}
