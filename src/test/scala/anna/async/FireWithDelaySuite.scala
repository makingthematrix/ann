package anna.async

import anna.async.NetBuilderOps._
import anna.data.{FireWithDelayBlock, HushValue}
import anna.logger.LOG.debug
import org.junit.Test
import org.junit.Assert._


/**
  * Created by gorywoda on 1/31/16.
  */
class FireWithDelaySuite extends MySuite {
  private def fireWithDelayData(delay: Double, inputTickMultiplier: Double = 3.0, defSlope: Double = 5.0) = {
    val builder = NetBuilder()
    builder.inputTickMultiplier = inputTickMultiplier
    builder.defSlope = defSlope
    val coeff = delay * inputTickMultiplier * 1.55 // a magic number to counteract inherent delays in sending and receiving messages
    builder.addInput("in").chain("mi11", 1.0, 0.0, HushValue(coeff.toInt)).hush("mi11")
      .chain("mi12", 1.0, 0.01).connect("mi12",1.0)//.loop("loop", 1.0, 0.0, 1.0)
      .chain("dot", 0.9/coeff, 0.9)
      .hush("mi12").hush("dot")
      .data
  }

  @Test def shouldResultInLongerDelays(): Unit = {
    build(fireWithDelayData(3.0))
    netWrapper.addAfterFire("dot")( (_:Double)=>{ assertEquals(3, netWrapper.iteration) } )
    netWrapper.tickUntilCalm("1")
    shutdown()

    build(fireWithDelayData(2.0))
    netWrapper.addAfterFire("dot")( (_:Double)=>{ assertEquals(2, netWrapper.iteration) } )
    netWrapper.tickUntilCalm("1")
    shutdown()

    build(fireWithDelayData(1.0))
    netWrapper.addAfterFire("dot")( (_:Double)=>{ assertEquals(1, netWrapper.iteration) } )
    netWrapper.tickUntilCalm("1")
    shutdown()

    build(fireWithDelayData(0.0))
    netWrapper.addAfterFire("dot")( (_:Double)=>{ assertEquals(0, netWrapper.iteration) } )
    netWrapper.tickUntilCalm("1")
    shutdown()
  }

  private def fireWithOps(blockName: String, delay: Double, inputTickMultiplier: Double = 3.0, defSlope: Double = 5.0) = {
    val builder = NetBuilder()
    builder.inputTickMultiplier = inputTickMultiplier
    builder.defSlope = defSlope
    builder.addInput("in").fireWithDelay(blockName, delay).data
  }

  @Test def shouldFireWithOps(): Unit = {
    build(fireWithOps("fireWithDelay",3.0))
    netWrapper.addAfterFire("fireWithDelay_out")( (_:Double)=>{ assertEquals(3, netWrapper.iteration) } )
    netWrapper.tickUntilCalm("1")
    shutdown()

    build(fireWithOps("fireWithDelay",2.0))
    netWrapper.addAfterFire("fireWithDelay_out")( (_:Double)=>{ assertEquals(2, netWrapper.iteration) } )
    netWrapper.tickUntilCalm("1")
    shutdown()

    build(fireWithOps("fireWithDelay",1.0))
    netWrapper.addAfterFire("fireWithDelay_out")( (_:Double)=>{ assertEquals(1, netWrapper.iteration) } )
    netWrapper.tickUntilCalm("1")
    shutdown()

    build(fireWithOps("fireWithDelay",0.0))
    netWrapper.addAfterFire("fireWithDelay_out")( (_:Double)=>{ assertEquals(0, netWrapper.iteration) } )
    netWrapper.tickUntilCalm("1")
    shutdown()
  }

  private def fireWithBlock(blockName: String, delay: Double, inputTickMultiplier: Double = 3.0, defSlope: Double = 5.0) = {
    FireWithDelayBlock(blockName, delay, inputTickMultiplier, defSlope)
    val builder = NetBuilder()
    builder.inputTickMultiplier = inputTickMultiplier
    builder.defSlope = defSlope
    builder.addInput("in")

    val block = FireWithDelayBlock(blockName, delay)
    block.chain(builder)
    (builder.data, block.outputId)
  }

  @Test def shouldFireWithBlock(): Unit = {
    val (block3, outputId3) = fireWithBlock("fireWithDelay",3.0)
    build(block3)
    netWrapper.addAfterFire(outputId3)( (_:Double)=>{ assertEquals(3, netWrapper.iteration) } )
    netWrapper.tickUntilCalm("1")
    shutdown()

    val (block2, outputId2) = fireWithBlock("fireWithDelay",2.0)
    build(block2)
    netWrapper.addAfterFire(outputId2)( (_:Double)=>{ assertEquals(2, netWrapper.iteration) } )
    netWrapper.tickUntilCalm("1")
    shutdown()

    val (block1, outputId1) = fireWithBlock("fireWithDelay",1.0)
    build(block1)
    netWrapper.addAfterFire(outputId1)( (_:Double)=>{ assertEquals(1, netWrapper.iteration) } )
    netWrapper.tickUntilCalm("1")
    shutdown()

    val (block0, outputId0) = fireWithBlock("fireWithDelay",0.0)
    build(block0)
    netWrapper.addAfterFire(outputId0)( (_:Double)=>{ assertEquals(0, netWrapper.iteration) } )
    netWrapper.tickUntilCalm("1")
    shutdown()
  }
}
