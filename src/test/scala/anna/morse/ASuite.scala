package anna.morse

import anna.blocks.Sequencer
import org.scalatest.junit.JUnitSuite
import anna.async.NetBuilderOps._
import org.junit.Assert._
import org.junit.Test

class ASuite extends JUnitSuite {
  import DotLine._

  private def buildA() = builder.sequencer("A", dotOut, lineOut).build()

  val aOut = Sequencer.outputId("A")

  @Test def shouldFireOnA() = {
    val net = buildA()

    var fired = false
    net.addAfterFire(aOut){ fired = true }
    net.iterateUntilCalm("1,0,0,0,1,1,0,0")
    assertTrue(fired)

    fired = false
    net.iterateUntilCalm("1,1,0,0,1,1,0,0")
    assertFalse(fired)

    net.reset()

    net.iterateUntilCalm("1,0,0,0,1,0,0,0")
    assertFalse(fired)

    net.reset()

    net.iterateUntilCalm("1,1,0,0,1,0,0,0")
    assertFalse(fired)

    net.shutdown()
  }
}
