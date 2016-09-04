package anna.blocks

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

    val inputVector = List.fill(requiredSignals)("1").mkString(",")
    LOG.debug("inputVector: " + inputVector)
    netWrapper.tickUntilCalm(inputVector)
    assertTrue(fired)

    if(requiredSignals > 1) {
      fired = false

      val inputVectorMinus1 = List.fill(requiredSignals - 1)("1").mkString(",")
      LOG.debug("inputVectorMinus1: " + inputVectorMinus1)
      netWrapper.tickUntilCalm(inputVectorMinus1)
      assertFalse(fired)
    }
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

  /*

  The idea is that after a certain interval the block should "renew" itself, so to the next signal it would react
  just as if it was the first signal ever. Currently though ConstantCurrent substracts from this new signal
  the whole amount of ForgetValue * interval interations, which usually makes it zero. Only the next signal after that
  is handled properly.

  @Test def cc2ShouldRenew(): Unit = {
    val (data, block) = constantCurrentWithBlock(2)
    build(data)

    var fired = false
    netWrapper.addAfterFire(block.outputId)( (_:Double)=>{ fired = true } )

    netWrapper.tickUntilCalm("1,0,0,1,1")
    assertTrue(fired)

    shutdown()
  }
  */
}
