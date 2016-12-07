package anna.blocks

import anna.async.NetBuilder
import anna.async.NetBuilderOps._
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite


/**
  * Created by gorywoda on 1/31/16.
  */
class DelayGateSuite extends JUnitSuite {

  private def delayGateWithOps(blockName: String, delay: Int) =
    NetBuilder().addInput("in").delayGate(blockName, delay).build()

  private def assertDelayGateWithOps(delay: Int) = {
    val netWrapper = delayGateWithOps("delayGate",delay)
    var iteration = 0
    LOG.allow("delayGate3")
    netWrapper.addAfterFire("delayGate3"){ iteration =  netWrapper.iteration }
    netWrapper.iterateUntilCalm("1")

    assertEquals(delay, iteration-1)

    netWrapper.shutdown()
  }

  @Test def shouldDelayGateWithOps(): Unit = {
    assertDelayGateWithOps(3)
    assertDelayGateWithOps(2)
    assertDelayGateWithOps(1)
    assertDelayGateWithOps(0)
  }

  private def delayGateWithBlock(delay: Int) = {
    val builder = NetBuilder()
    builder.addInput("in")

    val block = DelayGate(delay)
    block.chain(builder)
    (builder.build(), block)
  }

  private def shouldFireWithBlock(expectedDelay: Int) = {
    val (netWrapper, block) = delayGateWithBlock(expectedDelay)

    var fired = false
    var iteration = 0
    netWrapper.addAfterFire(block.outputId){
      LOG.debug(s"iteration: ${netWrapper.iteration}, ${block.outputId} fired")
      iteration = netWrapper.iteration
      fired = true
    }
    netWrapper.iterateUntilCalm("1")

    assertTrue(fired)
    assertEquals(expectedDelay, iteration-1)

    netWrapper.shutdown()
  }

  @Test def shouldFireWithBlock(): Unit = {
   for(i <- 0 to 5){
      shouldFireWithBlock(i)
   }
  }

}
