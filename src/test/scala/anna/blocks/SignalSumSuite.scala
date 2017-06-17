package anna.blocks

import anna.async.NetBuilderOps._
import anna.async.{NetBuilder, NetWrapper}
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

/**
  * Created by gorywoda on 6/19/16.
  */
class SignalSumSuite extends JUnitSuite {

  private def signalSumWithOps(blockName: String, requiredSignals: Int) =
    NetBuilder().addInput("in").signalSum(blockName, requiredSignals).build()

  private def assertFiredAfterRequiredSignals(netWrapper: NetWrapper, outNeuronId: String, requiredSignals: Int) = {
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
    val netWrapper = signalSumWithOps("SignalSum", 3)
    assertFiredAfterRequiredSignals(netWrapper, "SignalSum2", 3)
    netWrapper.shutdown()
  }

  private def signalSumWithBlock(requiredSignals: Int) = {
    val builder = NetBuilder()
    builder.addInput("in")

    val block = SignalSum(requiredSignals)
    block.chain(builder)
    (builder.build(), block)
  }

  private def shouldFireAfterRequiredSignalsBlock(requiredSignals: Int) = {
    val (netWrapper, block) = signalSumWithBlock(requiredSignals)
    assertFiredAfterRequiredSignals(netWrapper, block.outputId, requiredSignals)
    netWrapper.shutdown()
  }

  @Test def shouldFireWithBlock(): Unit = {
    shouldFireAfterRequiredSignalsBlock(3)
    shouldFireAfterRequiredSignalsBlock(2)
    shouldFireAfterRequiredSignalsBlock(1)
  }


}
