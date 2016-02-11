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
    shutdown()
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

  private def assertFireWithDelayData(delay: Int) = {
    build(fireWithDelayData(delay))
    var iteration = 0
    netWrapper.addAfterFire("dot")( (_:Double)=>{ iteration =  netWrapper.iteration } )
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertEquals(delay, iteration)
  }

  @Test def shouldResultInLongerDelays(): Unit = {
    assertFireWithDelayData(3)
    assertFireWithDelayData(2)
    assertFireWithDelayData(1)
    assertFireWithDelayData(0)
  }

  private def fireWithOps(blockName: String, delay: Double, inputTickMultiplier: Double = 3.0, defSlope: Double = 5.0) = {
    val builder = NetBuilder()
    builder.inputTickMultiplier = inputTickMultiplier
    builder.defSlope = defSlope
    builder.addInput("in").fireWithDelay(blockName, delay).data
  }

  private def assertFireWithOps(delay: Int) = {
    build(fireWithOps("fireWithDelay",delay))
    var iteration = 0
    netWrapper.addAfterFire("fireWithDelay_out")( (_:Double)=>{ iteration =  netWrapper.iteration } )
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertEquals(delay, iteration)
  }

  @Test def shouldFireWithOps(): Unit = {
    assertFireWithOps(3)
    assertFireWithOps(2)
    assertFireWithOps(1)
    assertFireWithOps(0)
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
    var iteration = 0
    netWrapper.addAfterFire(outputId)( (_:Double)=>{ iteration = netWrapper.iteration; fired = true } )
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertTrue(fired)
    assertEquals(expectedDelay, iteration)
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
  }

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
    var iteration = 0
    netWrapper.addAfterFire("out")( (_:Double)=>{ iteration = netWrapper.iteration; fired = true })
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertTrue(fired)
    assertEquals(delay + 1, iteration)
  }

  // @todo: shouldModifyFWDBlock with a mutation

  @Test def shouldModifyWDBlock(): Unit = {
    val ng = prepareFireWithDelayNet(2)
    build(ng.data)
    var fired = false
    var iteration = 0
    netWrapper.addAfterFire("out")( (_:Double)=>{ iteration = netWrapper.iteration; fired = true })
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertTrue(fired)
    assertEquals(3, iteration)

    Context.withFwdDelay(4)
    MutationsLibrary.mutate(ng, "modifyFireWithDelay")
    build(ng.data)
    fired = false
    iteration = 0
    netWrapper.addAfterFire("out")( (_:Double)=>{ iteration = netWrapper.iteration; fired = true })
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertTrue(fired)
    assertEquals(5, iteration)
  }
}
