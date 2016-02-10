package anna.epengine

import anna.Context
import anna.async.NetBuilderOps._
import anna.async.{MySuite, NetBuilder}
import anna.data.{SynapseWeight, HushValue}
import anna.logger.LOG
import org.junit.Assert._
import org.junit.{After, Before, Test}


/**
  * Created by gorywoda on 1/31/16.
  */
class FireWithDelaySuite extends MySuite {
  private var _oldContext:Context = _

  @Before override def before() {
    super.before()
    _oldContext = Context()
    LOG.addLogToStdout()
  }

  @After override def after(): Unit ={
    super.after()
    Context.set(_oldContext)
  }

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
/*
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

  private def shouldFireWithBlock(expectedDelay: Int) = {
    val (block, outputId) = fireWithBlock("fireWithDelay", expectedDelay)
    build(block)
    var fired = false
    netWrapper.addAfterFire(outputId)( (_:Double)=>{ assertEquals(expectedDelay, netWrapper.iteration); fired = true } )
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertTrue(fired)
  }

  @Test def shouldFireWithBlock(): Unit = {
    shouldFireWithBlock(3)
    shouldFireWithBlock(2)
    shouldFireWithBlock(1)
    shouldFireWithBlock(0)
  }

  @Test def shouldAddFWDBlock(): Unit = {
    val netDataInOut = NetBuilder().addInput("in").chain("out",1.0,0.0).netId("net").data

    val ng = NetGenome(netDataInOut, AccessMap(List("in"), List("out")))
    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in"))
    assertNotEquals(None, ng.find("out"))

    MutationsLibrary.mutate(ng, "addFireWithDelay")

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
    val netDataInOut = NetBuilder().addInput("in").chain("out",1.0,0.0).netId("net").data

    val ng = NetGenome(netDataInOut, AccessMap(List("in"), List("out")))
    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in"))
    assertNotEquals(None, ng.find("out"))

    MutationsLibrary.mutate(ng, "addFireWithDelay")

    assertEquals(2 + FireWithDelayBlock.neuronsInBlock, ng.neurons.size)
    val blockNames = FireWithDelayBlock.blocksInGenome(ng)
    assertEquals(1, blockNames.size)

    MutationsLibrary.mutate(ng, "deleteFireWithDelay")

    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in"))
    val in = ng.find("in").get
    assertNotEquals(None, ng.find("out"))
    assertTrue(in.isConnectedTo("out"))
  }*/

  private def prepareFireWithDelayNet(delay: Int) = {
    val netDataInOut = NetBuilder().addInput("in").chain("out",1.0,0.0).netId("net").data
    val ng = NetGenome(netDataInOut, AccessMap(List("in"), List("out")))

    Context.withFwdDelay(delay)
    MutationsLibrary.mutate(ng, "addFireWithDelay")
    assertEquals(2 + FireWithDelayBlock.neuronsInBlock, ng.neurons.size)

    val blockNames = FireWithDelayBlock.blocksInGenome(ng)
    assertEquals(1, blockNames.size)
    val blockName = blockNames.head

    val blockNameIn = blockName + "in"
    if(!ng.isConnected("in", blockNameIn)) {
      ng.connect("in", blockNameIn).get.weight = SynapseWeight(1.0)
      LOG.debug(this, s"connected in and $blockNameIn with ${ng.findSynapse("in", blockNameIn).weight}")
    } else {
      ng.findSynapse("in", blockNameIn).weight = SynapseWeight(1.0)
      LOG.debug(this, s"set the synapse between in and $blockNameIn to ${ng.findSynapse("in", blockNameIn).weight}")
    }

    if(ng.isConnected("out", blockNameIn)){
      ng.find("out").get.deleteSynapse(blockNameIn)
      LOG.debug(this, s"deleted the synapse between out and $blockNameIn")
    }

    if(ng.isConnected("in", "out")){
      ng.find("in").get.deleteSynapse("out")
      LOG.debug(this, s"deleted the synapse between in and out")
    }

    if(ng.isConnected("in", blockName + "hush")){
      ng.find("in").get.deleteSynapse(blockName + "hush")
      LOG.debug(this, s"deleted the synapse between in and ${blockName}hush")
    }

    ng
  }

  @Test def shouldUseFWDBlockInNet(): Unit = {
    val delay = 3
    val ng = prepareFireWithDelayNet(delay)

    build(ng.data)

    var fired = false
    netWrapper.addAfterFire("out")( (_:Double)=>{ assertEquals(delay + 1, netWrapper.iteration); fired = true })
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertTrue(fired)
  }

  // @todo: shouldModifyFWDBlock with a mutation
}
