package anna.epengine

import anna.async.{MySuite, NetBuilder}
import org.junit.Test
import anna.async.NetBuilderOps._
import anna.logger.LOG
import org.junit.Assert._

/**
  * Created by gorywoda on 7/2/16.
  */
class SOSWithBlocksSuite extends MySuite {

  private def buildDot(afterFireTrigger: (Double) => Any) = {
    val dotBlockName = "DotBlock"
    val expectedDelay = 3
    val outputId = DelayGate.outputId(dotBlockName)

    super.build(NetBuilder().addInput("in").delayGate(dotBlockName, expectedDelay).data)
    assertNotNull(netWrapper.find(outputId))

    netWrapper.addAfterFire(outputId)(afterFireTrigger)

    outputId
  }

  @Test def shouldFireOnDot(): Unit = {
    var fired = false
    var iteration = 0
    buildDot( (_:Double)=>{
      iteration = netWrapper.iteration
      fired = true
    })

    netWrapper.tickUntilCalm("1")

    assertTrue(fired)
    assertEquals(3, iteration)
  }

  private def buildLine(afterFireTrigger: (Double) => Any) = {
    val lineBlockName = "LineBlock"
    val requiredSignals = 2
    val outputId = DelayGate.outputId(lineBlockName)

    super.build(NetBuilder().addInput("in").signalSum(lineBlockName, 2).data)
    assertNotNull(netWrapper.find(outputId))

    netWrapper.addAfterFire(outputId)(afterFireTrigger)

    outputId
  }

  @Test def shouldFireOnLine(): Unit = {
    var fired = false

    val outputId = buildLine((_:Double)=>{ fired = true })

    for(i <- 1 until 2){
      netWrapper.tick('1')
      assertFalse(fired)
    }

    netWrapper.tick('1')
    assertTrue(fired)

    fired = false
    netWrapper.tick('1')
    assertFalse(fired)
  }

  @Test def shouldFireOnLine_simpleVersion(): Unit = {
    var fired = false

    val outputId = buildLine((_:Double)=>{ fired = true })

    netWrapper.tickUntilCalm("1,1")
    assertTrue(fired)

    fired = false
    netWrapper.tickUntilCalm("1")
    assertFalse(fired)
  }

  private class DotLineResults {
    var dotFired = 0
    var lineFired = 0
  }

  private def buildDotLine() = {
    val dotBlockName = "DotBlock"
    val expectedDelay = 3
    val dotOutputId = DelayGate.outputId(dotBlockName)
    val dotHushId = DelayGate.hushId(dotBlockName)
    val dotInputId = DelayGate.inputId(dotBlockName)

    val lineBlockName = "LineBlock"
    val requiredSignals = 2
    val lineOutputId = SignalSum.outputId(lineBlockName)
    val lineHushId = SignalSum.hushId(lineBlockName)
    val lineInputId = DelayGate.inputId(lineBlockName)

    super.build(
      NetBuilder().addInput("in").delayGate(dotBlockName, expectedDelay).use("in").signalSum(lineBlockName, requiredSignals)
        .use(dotOutputId).hush(lineHushId).use(lineOutputId).hush(dotHushId).data
    )

    LOG.allow(dotOutputId)
    LOG.allow(lineOutputId)
    LOG.allow(dotHushId)
    LOG.allow(lineHushId)
    LOG.allow(dotInputId)
    LOG.allow(lineInputId)
    LOG.allow("SBlockmi")

    val results = new DotLineResults;

    netWrapper.addAfterFire("in")((_:Double) =>{
      LOG.debug(s"${netWrapper.iteration}: Incoming!")
    })

    netWrapper.addAfterFire(dotOutputId)((_:Double)=>{
      LOG.debug(s"${netWrapper.iteration}: Dot!")
      results.dotFired += 1
    })

    netWrapper.addAfterFire(lineOutputId)((_:Double)=>{
      LOG.debug(s"${netWrapper.iteration}: Line!")
      results.lineFired += 1
    })

    results
  }

  @Test def shouldDotWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.tickUntilCalm("1")
    assertEquals(1, results.dotFired)
    assertEquals(0, results.lineFired)
  }

  @Test def shouldLineWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.tickUntilCalm("1,1")
    assertEquals(0, results.dotFired)
    assertEquals(1, results.lineFired)
  }

  @Test def shouldTwoDotsWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.tickUntilCalm("1,0,0,1")
    assertEquals(2, results.dotFired)
    assertEquals(0, results.lineFired)
  }

}
