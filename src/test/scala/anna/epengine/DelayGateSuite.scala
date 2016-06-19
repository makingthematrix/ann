package anna.epengine

import anna.Context
import anna.async.NetBuilderOps._
import anna.async.{MySuite, NetBuilder}
import anna.data.SynapseWeight
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
    netWrapper.tickUntilCalm("1")
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

  @Test def shouldAddFWDBlock(): Unit = {
    val netDataInOut = NetBuilder().addInput("in").chain("out",1.0,0.0).netId("net").data

    val ng = NetGenome(netDataInOut, AccessMap(List("in"), List("out")))
    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in"))
    assertNotEquals(None, ng.find("out"))

    MutationsLibrary.mutate(ng, "addDelayGate")

    assertEquals(2 + DelayGate.neuronsInBlock, ng.neurons.size)
    val blockNames = DelayGate.blocksInGenome(ng)
    assertEquals(1, blockNames.size)
    val blockName = blockNames.head

    val inputId = DelayGate.inputId(blockName)
    assertNotEquals(None, ng.find(inputId))
    val ins = ng.findIdsConnectedTo(inputId)
    assertTrue(ins.contains("in") || ins.contains("out"))
    assertFalse(ins.contains("in") && ins.contains("out"))

    val outputId = DelayGate.outputId(blockName)
    val outputNeuronOpt = ng.find(outputId)
    assertNotEquals(None, ng.find(outputId))
    val outputNeuron = outputNeuronOpt.get
    assertTrue(outputNeuron.isConnectedTo("out"))

    val hushId = DelayGate.hushId(blockName)
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

    MutationsLibrary.mutate(ng, "addDelayGate")

    assertEquals(2 + DelayGate.neuronsInBlock, ng.neurons.size)
    val blockNames = DelayGate.blocksInGenome(ng)
    assertEquals(1, blockNames.size)

    MutationsLibrary.mutate(ng, "deleteDelayGate")

    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in"))
    val in = ng.find("in").get
    assertNotEquals(None, ng.find("out"))
    assertTrue(in.isConnectedTo("out"))
  }

  private def prepareDelayGateNet(delay: Int) = {
    val netDataInOut = NetBuilder().addInput("in").chain("out",1.0,0.0).netId("net").data
    val ng = NetGenome(netDataInOut, AccessMap(List("in"), List("out")))

    Context.withFwdDelay(delay)
    MutationsLibrary.mutate(ng, "addDelayGate")
    assertEquals(2 + DelayGate.neuronsInBlock, ng.neurons.size)

    val blockNames = DelayGate.blocksInGenome(ng)
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
    val ng = prepareDelayGateNet(delay)

    build(ng.data)

    var fired = false
    var iteration = 0
    netWrapper.addAfterFire("out")( (_:Double)=>{ iteration = netWrapper.iteration; fired = true })
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertTrue(fired)
    assertEquals(delay, iteration)
  }

  // @todo: shouldModifyFWDBlock with a mutation
/*
  @Test def shouldModifyWDBlock(): Unit = {
    val ng = prepareDelayGateNet(2)
    build(ng.data)
    var fired = false
    var iteration = 0
    netWrapper.addAfterFire("out")( (_:Double)=>{ iteration = netWrapper.iteration; fired = true })
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertTrue(fired)
    assertEquals(2, iteration)

    Context.withFwdDelay(4)
    MutationsLibrary.mutate(ng, "modifyDelayGate")
    build(ng.data)
    fired = false
    iteration = 0
    netWrapper.addAfterFire("out")( (_:Double)=>{ iteration = netWrapper.iteration; fired = true })
    netWrapper.tickUntilCalm("1")
    shutdown()
    assertTrue(fired)
    assertEquals(4, iteration)
  }*/
}
