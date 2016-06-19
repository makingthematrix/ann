package anna.epengine

import anna.async.NetBuilderOps._
import org.junit.Test
import anna.async.{MySuite, NetBuilder}
import anna.logger.LOG
import org.junit.Assert._

/**
  * Created by gorywoda on 6/19/16.
  */
class SignalSumSuite extends MySuite {
  private def signalSumWithOps(blockName: String, requiredSignals: Int) = NetBuilder().addInput("in").signalSum(blockName, requiredSignals).data

  private def assertFiredAfterRequiredSignals(outNeuronId: String, requiredSignals: Int) = {
    var fired = false
    netWrapper.addAfterFire(outNeuronId)( (_:Double)=>{ fired = true } )

    for(i <- 1 until requiredSignals){
      netWrapper.tick('1')
      assertFalse(fired)
    }

    netWrapper.tick('1')
    assertTrue(fired)

    fired = false
    netWrapper.tick('1')
    assertFalse(fired)
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
    for(i <- 1 to 5){
      shouldFireAfterRequiredSignalsBlock(i)
    }
  }


}
