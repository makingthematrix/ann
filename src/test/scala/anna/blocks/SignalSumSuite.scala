package anna.blocks

import anna.async.NetBuilderOps._
import anna.async.{MySuite, NetBuilder}
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test

/**
  * Created by gorywoda on 6/19/16.
  */
class SignalSumSuite extends MySuite {
  private def signalSumWithOps(blockName: String, requiredSignals: Int) = NetBuilder().addInput("in").signalSum(blockName, requiredSignals).data

  private def assertFiredAfterRequiredSignals(outNeuronId: String, requiredSignals: Int) = {
    var fired = false
    netWrapper.addAfterFire(outNeuronId){ fired = true }

    val inputVector = List.fill(requiredSignals)("1").mkString(",")
    LOG.debug("inputVector: " + inputVector)
    netWrapper.iterateUntilCalm(inputVector)
    assertTrue(fired)

    if(requiredSignals > 1) {
      fired = false

      val inputVectorMinus1 = List.fill(requiredSignals - 1)("1").mkString(",")
      LOG.debug("inputVectorMinus1: " + inputVectorMinus1)
      netWrapper.iterateUntilCalm(inputVectorMinus1)
      assertFalse(fired)
    }
  }

  @Test def shouldSignalSumWithOps(): Unit ={
    build(signalSumWithOps("SignalSum",3))
    assertFiredAfterRequiredSignals("SignalSumout",3)
  }

  private def signalSumWithBlock(requiredSignals: Int) = {
    val builder = NetBuilder()
    builder.addInput("in")

    val block = SignalSum(requiredSignals)
    block.chain(builder)
    (builder.data, block)
  }

  private def shouldFireAfterRequiredSignalsBlock(requiredSignals: Int) = {
    val (data, block) = signalSumWithBlock(requiredSignals)
    build(data)
    assertFiredAfterRequiredSignals(block.outputId, requiredSignals)
    shutdown()
  }

  @Test def shouldFireWithBlock(): Unit = {
    shouldFireAfterRequiredSignalsBlock(3)
    shouldFireAfterRequiredSignalsBlock(2)
    shouldFireAfterRequiredSignalsBlock(1)
  }


}
