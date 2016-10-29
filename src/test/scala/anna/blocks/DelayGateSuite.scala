package anna.blocks

import anna.async.NetBuilderOps._
import anna.async.{MySuite, NetBuilder}
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test


/**
  * Created by gorywoda on 1/31/16.
  */
class DelayGateSuite extends MySuite {
  private def delayGateWithOps(blockName: String, delay: Int) = NetBuilder().addInput("in").delayGate(blockName, delay).data

  private def assertDelayGateWithOps(delay: Int) = {
    build(delayGateWithOps("delayGate",delay))
    var iteration = 0
    netWrapper.addAfterFire("delayGateout")( (_:Double)=>{ iteration =  netWrapper.iteration } )
    netWrapper.iterateUntilCalm("1")
    shutdown()
    assertEquals(delay, iteration)
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
    (builder.data, block)
  }

  private def shouldFireWithBlock(expectedDelay: Int) = {
    val (data, block) = delayGateWithBlock(expectedDelay)
    build(data)
    var fired = false
    var iteration = 0
    netWrapper.addAfterFire(block.outputId)( (_:Double)=>{
      LOG.debug(s"iteration: ${netWrapper.iteration}, ${block.outputId} fired")
      iteration = netWrapper.iteration
      fired = true
    })
    netWrapper.iterateUntilCalm("1")
    shutdown()
    assertTrue(fired)
    assertEquals(expectedDelay, iteration)
  }

  @Test def shouldFireWithBlock(): Unit = {
   for(i <- 0 to 5){
      shouldFireWithBlock(i)
   }
  }

}
