package anna.epengine

import anna.async.{MySuite, NetBuilder}
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test
import anna.async.NetBuilderOps._

/**
  * Created by gorywoda on 7/10/16.
  */
class DelayGateWithBreakSuite extends MySuite {
  private def dgbWithBlock(delay: Int) = {
    val builder = NetBuilder()
    builder.addInput("in")

    val block = DelayGateWithBreak(delay)
    block.chain(builder)
    (builder.data, block)
  }

  private def shouldFireWithBlock(expectedDelay: Int) = {
    val (data, block) = dgbWithBlock(expectedDelay)
    build(data)
    var fired = false
    var iteration = 0
    netWrapper.addAfterFire(block.outputId)( (_:Double)=>{
      LOG.debug(s"iteration: ${netWrapper.iteration}, ${block.outputId} fired")
      iteration = netWrapper.iteration
      fired = true
    })
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertTrue(fired)
    assertEquals(expectedDelay, iteration)
  }

  @Test def shouldFireWithBlock(): Unit = {
    for(i <- 0 to 5){
      shouldFireWithBlock(i)
    }
  }

  private def dgbWithOps(blockName: String, delay: Int) = NetBuilder().addInput("in").delayGateWithBreak(blockName, delay).data

  private def assertDGBWithOps(delay: Int) = {
    build(dgbWithOps("delayGateWithBreak",delay))
    var iteration = 0
    netWrapper.addAfterFire("delayGateWithBreakinnerDGout")( (_:Double)=>{ iteration =  netWrapper.iteration } )
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertEquals(delay, iteration)
  }

  @Test def shouldDGBWithOps(): Unit = {
    assertDGBWithOps(3)
    assertDGBWithOps(2)
    assertDGBWithOps(1)
    assertDGBWithOps(0)
  }

  @Test def shouldNotMistakeLineForDot(): Unit = {
    val (data, block) = dgbWithBlock(3)
    build(data)
    var fired = false
    netWrapper.addAfterFire(block.outputId)( (_:Double)=>{
      fired = true
    })

    netWrapper.tickUntilCalm("1")
    assertTrue(fired)

    fired = false
    netWrapper.tickUntilCalm("1,1")
    assertFalse(fired)

    netWrapper.tickUntilCalm("1")
    assertTrue(fired)

  }
}
