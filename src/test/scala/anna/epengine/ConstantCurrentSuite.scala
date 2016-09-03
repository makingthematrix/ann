package anna.epengine

import anna.async.NetBuilderOps._
import anna.async.{MySuite, NetBuilder}
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test

/**
  * Created by gorywoda on 9/3/16.
  */
class ConstantCurrentSuite extends MySuite {
  private def constantCurrentWithOps(blockName: String, requiredSignals: Int) = NetBuilder().addInput("in").constantCurrent(blockName, requiredSignals).data

  private def assertFiredAfterRequiredSignals(outNeuronId: String, requiredSignals: Int) = {
    var fired = false
    netWrapper.addAfterFire(outNeuronId)( (_:Double)=>{ fired = true } )

    for(i <- 1 until requiredSignals){
      netWrapper.tick('1')
      assertFalse(fired)
    }

    netWrapper.tick('1')
    assertTrue(fired)

    fired = false
    netWrapper.tick('1')
    assertFalse(fired)
  }

  @Test def shouldSignalSumWithOps(): Unit ={
    build(constantCurrentWithOps("CC",3))
    assertFiredAfterRequiredSignals("CCout",3)
  }

  private def constantCurrentWithBlock(requiredSignals: Int) = {
    val builder = NetBuilder()
    builder.addInput("in")

    val block = ConstantCurrent(requiredSignals)
    LOG.allow(block.inputId, block.outputId, block.hushId)
    block.chain(builder)
    (builder.data, block)
  }

  private def shouldFireAfterRequiredSignalsBlock(requiredSignals: Int) = {
    val (data, block) = constantCurrentWithBlock(requiredSignals)
    build(data)
    assertFiredAfterRequiredSignals(block.outputId, requiredSignals)
    shutdown()
  }

  @Test def shouldFireWithBlock3(): Unit = {
    shouldFireAfterRequiredSignalsBlock(3)
  }

  @Test def shouldFireWithBlock4(): Unit = {
    shouldFireAfterRequiredSignalsBlock(4)
  }

  @Test def shouldFireWithBlock5(): Unit = {
    shouldFireAfterRequiredSignalsBlock(5)
  }

  @Test def shouldNotFireIfHoleOccured(): Unit = {
    val (data, block) = constantCurrentWithBlock(3)
    build(data)

    var fired = false
    netWrapper.addAfterFire(block.outputId)( (_:Double)=>{ fired = true } )

    netWrapper.tickUntilCalm("1,1,1")
    assertTrue(fired)

    fired = false
    netWrapper.tickUntilCalm("1,1")
    assertFalse(fired)

    netWrapper.tickUntilCalm("1,1,0,1")
    assertFalse(fired)

    shutdown()
  }

  @Test def cc1ShouldJustFire(): Unit = {
    val (data, block) = constantCurrentWithBlock(1)
    build(data)

    var fired = false
    netWrapper.addAfterFire(block.outputId)( (_:Double)=>{ fired = true } )

    netWrapper.tickUntilCalm("1")
    assertTrue(fired)

    shutdown()
  }

  @Test def cc2ShouldFireFor2Signals(): Unit = {
    val (data, block) = constantCurrentWithBlock(2)
    build(data)

    var fired = false
    netWrapper.addAfterFire(block.outputId)( (_:Double)=>{ fired = true } )

    netWrapper.tickUntilCalm("1,1")
    assertTrue(fired)

    shutdown()
  }

  @Test def cc2ShouldNotFireForOscillatingSignals(): Unit = {
    val (data, block) = constantCurrentWithBlock(2)
    build(data)

    var fired = false
    netWrapper.addAfterFire(block.outputId)( (_:Double)=>{ fired = true } )

    netWrapper.tickUntilCalm("1,0,1")
    assertFalse(fired)

    netWrapper.tickUntilCalm("1,0,1,0,1,0,1,0,1,0,1")
    assertFalse(fired)

    shutdown()
  }

  @Test def cc2ShouldRenew(): Unit = {
    val (data, block) = constantCurrentWithBlock(2)
    build(data)

    var fired = false
    netWrapper.addAfterFire(block.outputId)( (_:Double)=>{ fired = true } )

    netWrapper.tickUntilCalm("1,0,0,1,1")
    assertTrue(fired)

    shutdown()
  }
}
