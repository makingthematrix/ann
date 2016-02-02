package anna.epengine

import anna.async.NetBuilderOps._
import anna.async.{MySuite, NetBuilder}
import anna.data.HushValue
import org.junit.Assert._
import org.junit.Test


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
      .chain("mi12", 1.0, 0.01).connect("mi12",1.0)
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

  private def fireWithBlock(blockName: String, delay: Double, inputTickMultiplier: Double = 3.0) = {
    val builder = NetBuilder()
    builder.inputTickMultiplier = inputTickMultiplier
    builder.addInput("in")

    val block = FireWithDelayBlock(blockName, delay, inputTickMultiplier)
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

  @Test def shouldAddFWDBlock(): Unit = {
    val netDataInOut = NetBuilder().addInput("in").chain("out",1.0,1.0).netId("net").data

    val ng = NetGenome(netDataInOut, AccessMap(List("in"), List("out")))
    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in"))
    assertNotEquals(None, ng.find("out"))

    MutationsProfile(
      "addFireWithDelay" -> 1.0
    ).mutate(ng)

    assertEquals(2 + FireWithDelayBlock.neuronsInBlock, ng.neurons.size)
    val blockNames = FireWithDelayBlock.blocksInGenome(ng)
    assertEquals(1, blockNames.size)
    val blockName = blockNames.head

    val inputId = FireWithDelayBlock.inputId(blockName)
    assertNotEquals(None, ng.find(inputId))
    val ins = ng.findIdsConnectedTo(inputId)
    assertTrue(ins.contains("in") || ins.contains("out"))
    assertFalse(ins.contains("in") && ins.contains("out"))

    val outputId = FireWithDelayBlock.outputId(blockName)
    val outputNeuronOpt = ng.find(outputId)
    assertNotEquals(None, ng.find(outputId))
    val outputNeuron = outputNeuronOpt.get
    assertTrue(outputNeuron.isConnectedTo("out"))

    val hushId = FireWithDelayBlock.hushId(blockName)
    assertNotEquals(None, ng.find(hushId))
    val hushes = ng.findIdsConnectedTo(hushId)
    assertTrue(hushes.contains("in") || hushes.contains("out"))
    assertFalse(hushes.contains("in") && hushes.contains("out"))
  }

  @Test def shouldDeleteFWDBlock(): Unit = {
    val netDataInOut = NetBuilder().addInput("in").chain("out",1.0,1.0).netId("net").data

    val ng = NetGenome(netDataInOut, AccessMap(List("in"), List("out")))
    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in"))
    assertNotEquals(None, ng.find("out"))

    MutationsProfile(
      "addFireWithDelay" -> 1.0
    ).mutate(ng)

    assertEquals(2 + FireWithDelayBlock.neuronsInBlock, ng.neurons.size)
    val blockNames = FireWithDelayBlock.blocksInGenome(ng)
    assertEquals(1, blockNames.size)

    MutationsProfile(
      "deleteFireWithDelay" -> 1.0
    ).mutate(ng)

    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in"))
    val in = ng.find("in").get
    assertNotEquals(None, ng.find("out"))
    assertTrue(in.isConnectedTo("out"))
  }
}
