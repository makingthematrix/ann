package anna.blocks

import anna.async.NetBuilderOpsTyped._
import anna.async.{NetBuilderTyped, NetWrapperTyped}
import anna.logger.LOG
import org.junit.Assert._
import org.scalatest.funsuite.AnyFunSuite

/**
 * Typed-Actor counterpart of [[SignalSumSuite]], validating [[SignalSumTyped]].
 */
class SignalSumTypedSuite extends AnyFunSuite {

  private def signalSumWithOps(blockName: String, requiredSignals: Int): NetWrapperTyped =
    NetBuilderTyped().addInput("in").signalSum(blockName, requiredSignals).build()

  private def assertFiredAfterRequiredSignals(netWrapper: NetWrapperTyped, outNeuronId: String, requiredSignals: Int): Unit = {
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

  private def signalSumWithBlock(requiredSignals: Int): (NetWrapperTyped, SignalSumTyped) = {
    val builder = NetBuilderTyped()
    builder.addInput("in")

    val block = SignalSumTyped(requiredSignals)
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
