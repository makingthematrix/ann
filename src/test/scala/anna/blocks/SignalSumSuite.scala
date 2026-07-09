package anna.blocks

import anna.async.NetBuilderOps._
import anna.async.{NetBuilder, NetWrapper}
import anna.logger.LOG
import org.junit.Assert._
import org.scalatest.funsuite.AnyFunSuite

/**
 * Tests validating the [[SignalSum]] block.
 */
class SignalSumSuite extends AnyFunSuite {

  private def signalSumWithOps(blockName: String, requiredSignals: Int): NetWrapper =
    NetBuilder().addInput("in").signalSum(blockName, requiredSignals).build()

  private def assertFiredAfterRequiredSignals(netWrapper: NetWrapper, outNeuronId: String, requiredSignals: Int): Unit = {
    var fired = false
    netWrapper.addAfterFire(outNeuronId) { fired = true }

    val inputVector = List.fill(requiredSignals)("1").mkString(",")
    LOG.debug("inputVector: " + inputVector)
    netWrapper.iterateUntilCalm(inputVector)
    assertTrue(fired)

    if (requiredSignals > 1) {
      fired = false

      val inputVectorMinus1 = List.fill(requiredSignals - 1)("1").mkString(",")
      LOG.debug("inputVectorMinus1: " + inputVectorMinus1)
      netWrapper.iterateUntilCalm(inputVectorMinus1)
      assertFalse(fired)
    }
  }

  test("shouldSignalSumWithOps") {
    val netWrapper = signalSumWithOps("SignalSum", 3)
    assertFiredAfterRequiredSignals(netWrapper, "SignalSum2", 3)
    netWrapper.shutdown()
  }

  private def signalSumWithBlock(requiredSignals: Int): (NetWrapper, SignalSum) = {
    val builder = NetBuilder()
    builder.addInput("in")

    val block = SignalSum(requiredSignals)
    block.chain(builder)
    (builder.build(), block)
  }

  private def shouldFireAfterRequiredSignalsBlock(requiredSignals: Int): Unit = {
    val (netWrapper, block) = signalSumWithBlock(requiredSignals)
    assertFiredAfterRequiredSignals(netWrapper, block.outputId, requiredSignals)
    netWrapper.shutdown()
  }

  test("shouldFireWithBlock") {
    shouldFireAfterRequiredSignalsBlock(3)
    shouldFireAfterRequiredSignalsBlock(2)
    shouldFireAfterRequiredSignalsBlock(1)
  }
}
