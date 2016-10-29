package anna.blocks

import anna.async.NetBuilderOps._
import anna.async.{MySuite, NetBuilder}
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test

/**
  * Created by gorywoda on 7/2/16.
  */
class SOSWithBlocksSuite extends MySuite {

  private def buildDot(afterFireTrigger: (Double) => Any) = {
    val dotBlockName = "DotBlock"
    val expectedDelay = 2
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

    netWrapper.iterateUntilCalm("1")

    assertTrue(fired)
    assertEquals(2, iteration)
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

    LOG.allow(outputId)

    netWrapper.iterateUntilCalm("1,1")
    assertTrue(fired)

    fired = false
    netWrapper.iterateUntilCalm("1")
    assertFalse(fired)
  }

  private class TestResults {
    var dotFired = 0
    var lineFired = 0
    var sFired = 0
    var oFired = 0
  }

  private def buildDotLine() = {
    val dotBlockName = "DotBlock"
    val expectedDelay = 2
    val dotOutputId = DelayGate.outputId(dotBlockName)
    val dotHushId = DelayGate.silencingId(dotBlockName)
    val dotInputId = DelayGate.inputId(dotBlockName)
    val dotMiddleId = DelayGate.middleId(dotBlockName)

    val lineBlockName = "LineBlock"
    val requiredSignals = 2
    val lineOutputId = SignalSum.outputId(lineBlockName)
    val lineHushId = SignalSum.silencingId(lineBlockName)
    val lineInputId = SignalSum.inputId(lineBlockName)

    super.build(
      NetBuilder().addInput("in")
        .delayGate(dotBlockName, expectedDelay)
        .use("in").signalSum(lineBlockName, requiredSignals)
        .use(dotOutputId).silence(lineHushId)
        .use(lineOutputId).silence(dotHushId)
        .data
    )

    LOG.allow(dotOutputId, lineOutputId, dotHushId, dotInputId, dotMiddleId)

    val results = new TestResults;

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
    netWrapper.iterateUntilCalm("1")
    assertEquals(1, results.dotFired)
    assertEquals(0, results.lineFired)
  }

  @Test def shouldLineWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.iterateUntilCalm("1,1")
    assertEquals(0, results.dotFired)
    assertEquals(1, results.lineFired)
  }

  @Test def shouldTwoDotsWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.iterateUntilCalm("1,0,0,0,1")
    assertEquals(2, results.dotFired)
    assertEquals(0, results.lineFired)
  }

  @Test def shouldLineAndDotWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.iterateUntilCalm("1,1,0,0,1")
    assertEquals(1, results.dotFired)
    assertEquals(1, results.lineFired)
  }

  @Test def shouldDotAndLineWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.iterateUntilCalm("1,0,0,0,1,1")
    assertEquals(1, results.dotFired)
    assertEquals(1, results.lineFired)
  }

  @Test def shouldThreeDotsWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.iterateUntilCalm("1,0,0,0,1,0,0,0,1")
    assertEquals(3, results.dotFired)
    assertEquals(0, results.lineFired)
  }

  @Test def shouldThreeLinesWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.iterateUntilCalm("1,1,0,0,1,1,0,0,1,1")
    assertEquals(0, results.dotFired)
    assertEquals(3, results.lineFired)
  }

  private def buildSO(outputBuffer:Option[StringBuilder] = None) = {
    val dotBlockName = "Dot-"
    val dotExpectedDelay = 2
    val dotOutputId = DelayGate.outputId(dotBlockName)
    val dotHushId = DelayGate.silencingId(dotBlockName)
    val dotInputId = DelayGate.inputId(dotBlockName)
    val dotMiddleId = DelayGate.middleId(dotBlockName)

    val lineBlockName = "Line-"
    val lineRequiredSignals = 2
    val lineOutputId = SignalSum.outputId(lineBlockName)
    val lineHushId = SignalSum.silencingId(lineBlockName)
    val lineInputId = SignalSum.inputId(lineBlockName)

    val sBlockName = "S-"
    val sRequiredSignals = 3
    val sOutputId = SignalSum.outputId(sBlockName)
    val sHushId = SignalSum.silencingId(sBlockName)
    val sInputId = SignalSum.inputId(sBlockName)

    val oBlockName = "O-"
    val oRequiredSignals = 3
    val oOutputId = SignalSum.outputId(oBlockName)
    val oHushId = SignalSum.silencingId(oBlockName)
    val oInputId = SignalSum.inputId(oBlockName)

    super.build(
      NetBuilder().addInput("in")
        .delayGate(dotBlockName, dotExpectedDelay)
        .use("in").signalSum(lineBlockName, lineRequiredSignals)
        .use(dotOutputId).silence(lineHushId)
        .use(lineOutputId).silence(dotHushId)
        .use(dotOutputId).signalSum(sBlockName, sRequiredSignals)
        .use(lineOutputId).signalSum(oBlockName, oRequiredSignals)
        .use(sOutputId).silence(oHushId)
        .use(oOutputId).silence(sHushId)
        .data
    )

    LOG.allow(dotOutputId, lineOutputId, dotHushId, lineHushId, dotInputId, lineInputId, dotMiddleId)

    val results = new TestResults;

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

    netWrapper.addAfterFire(sOutputId)((_:Double)=>{
      LOG.debug(s"${netWrapper.iteration}: S!")
      results.sFired += 1
      outputBuffer match {
        case Some(buffer) => buffer.append('S')
        case None =>
      }
    })

    netWrapper.addAfterFire(oOutputId)((_:Double)=>{
      LOG.debug(s"${netWrapper.iteration}: O!")
      results.oFired += 1
      outputBuffer match {
        case Some(buffer) => buffer.append('O')
        case None =>
      }
    })

    results
  }

  @Test def shouldSWithSO(): Unit = {
    val results = buildSO()
    netWrapper.iterateUntilCalm("1,0,0,0,1,0,0,0,1")
    assertEquals(3, results.dotFired)
    assertEquals(0, results.lineFired)
    assertEquals(1, results.sFired)
    assertEquals(0, results.oFired)
  }

  @Test def shouldOWithSO(): Unit = {
    val results = buildSO()
    netWrapper.iterateUntilCalm("1,1,0,0,1,1,0,0,1,1")
    assertEquals(0, results.dotFired)
    assertEquals(3, results.lineFired)
    assertEquals(0, results.sFired)
    assertEquals(1, results.oFired)
  }

  @Test def shouldSOWithSO(): Unit = {
    val results = buildSO()
    netWrapper.iterateUntilCalm("1,0,0,0,1,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1")
    assertEquals(3, results.dotFired)
    assertEquals(3, results.lineFired)
    assertEquals(1, results.sFired)
    assertEquals(1, results.oFired)
  }

  /* This is the test which proves it all works :) */

  @Test def shouldSOSWithSO(): Unit = {
    val buffer = StringBuilder.newBuilder
    val results = buildSO(Some(buffer))
    netWrapper.iterateUntilCalm("1,0,0,0,1,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,0,0,0,1,0,0,0,1")
    assertEquals(6, results.dotFired)
    assertEquals(3, results.lineFired)
    assertEquals(2, results.sFired)
    assertEquals(1, results.oFired)

    assertEquals("SOS", buffer.toString())
  }

}
