package anna.epengine

import anna.async.{MySuite, NetBuilder}
import org.junit.Test
import anna.async.NetBuilderOps._
import anna.data.HushValue
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

    netWrapper.tickUntilCalm("1,1")
    assertTrue(fired)

    fired = false
    netWrapper.tickUntilCalm("1")
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
    val expectedDelay = 3
    val dotOutputId = DelayGate.outputId(dotBlockName)
    val dotHushId = DelayGate.hushId(dotBlockName)
    val dotInputId = DelayGate.inputId(dotBlockName)
    val dotMiddleId = DelayGate.middleId(dotBlockName)

    val dot2BlockName = "DotBlock2"
    val dot2OutputId = DelayGate.outputId(dot2BlockName)
    val dot2HushId = DelayGate.hushId(dot2BlockName)

    val lineBlockName = "LineBlock"
    val requiredSignals = 2
    val lineOutputId = SignalSum.outputId(lineBlockName)
    val lineHushId = SignalSum.hushId(lineBlockName)
    val lineInputId = SignalSum.inputId(lineBlockName)

    super.build(
      NetBuilder().addInput("in")
        .delayGate(dotBlockName, expectedDelay)
        .use("in").signalSum(lineBlockName, requiredSignals)
        .use(dotOutputId).hush(lineHushId)
        .use(lineOutputId).hush(dotHushId)
        .data
    )

    LOG.allow(dotOutputId, lineOutputId, dotHushId)

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
    netWrapper.tickUntilCalm("1,0,0,0,1")
    assertEquals(2, results.dotFired)
    assertEquals(0, results.lineFired)
  }

  @Test def shouldLineAndDotWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.tickUntilCalm("1,1,0,0,1")
    assertEquals(1, results.dotFired)
    assertEquals(1, results.lineFired)
  }

  @Test def shouldDotAndLineWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.tickUntilCalm("1,0,0,0,1,1")
    assertEquals(1, results.dotFired)
    assertEquals(1, results.lineFired)
  }

  @Test def shouldThreeDotsWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.tickUntilCalm("1,0,0,0,1,0,0,0,1")
    assertEquals(3, results.dotFired)
    assertEquals(0, results.lineFired)
  }

  @Test def shouldThreeLinesWithDotLine(): Unit = {
    val results = buildDotLine()
    netWrapper.tickUntilCalm("1,1,0,0,1,1,0,0,1,1")
    assertEquals(0, results.dotFired)
    assertEquals(3, results.lineFired)
  }

  private def buildSO(outputBuffer:Option[StringBuilder] = None) = {
    val dotBlockName = "Dot-"
    val dotExpectedDelay = 3
    val dotOutputId = DelayGate.outputId(dotBlockName)
    val dotHushId = DelayGate.hushId(dotBlockName)
    val dotInputId = DelayGate.inputId(dotBlockName)
    val dotMiddleId = DelayGate.middleId(dotBlockName)

    val lineBlockName = "Line-"
    val lineRequiredSignals = 2
    val lineOutputId = SignalSum.outputId(lineBlockName)
    val lineHushId = SignalSum.hushId(lineBlockName)
    val lineInputId = DelayGate.inputId(lineBlockName)

    val sBlockName = "S-"
    val sRequiredSignals = 3
    val sOutputId = SignalSum.outputId(sBlockName)
    val sHushId = SignalSum.hushId(sBlockName)
    val sInputId = DelayGate.inputId(sBlockName)

    val oBlockName = "O-"
    val oRequiredSignals = 3
    val oOutputId = SignalSum.outputId(oBlockName)
    val oHushId = SignalSum.hushId(oBlockName)
    val oInputId = DelayGate.inputId(oBlockName)

    super.build(
      NetBuilder().addInput("in")
        .delayGate(dotBlockName, dotExpectedDelay)
        .use("in").signalSum(lineBlockName, lineRequiredSignals)
        .use(dotOutputId).hush(lineHushId)
        .use(lineOutputId).hush(dotHushId)
        .use(dotOutputId).signalSum(sBlockName, sRequiredSignals)
        .use(lineOutputId).signalSum(oBlockName, oRequiredSignals)
        .use(sOutputId).hush(oHushId)
        .use(oOutputId).hush(sHushId)
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
    netWrapper.tickUntilCalm("1,0,0,0,1,0,0,0,1")
    assertEquals(3, results.dotFired)
    assertEquals(0, results.lineFired)
    assertEquals(1, results.sFired)
    assertEquals(0, results.oFired)
  }

  @Test def shouldOWithSO(): Unit = {
    val results = buildSO()
    netWrapper.tickUntilCalm("1,1,0,0,1,1,0,0,1,1")
    assertEquals(0, results.dotFired)
    assertEquals(3, results.lineFired)
    assertEquals(0, results.sFired)
    assertEquals(1, results.oFired)
  }

  @Test def shouldSOWithSO(): Unit = {
    val results = buildSO()
    netWrapper.tickUntilCalm("1,0,0,0,1,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1")
    assertEquals(3, results.dotFired)
    assertEquals(3, results.lineFired)
    assertEquals(1, results.sFired)
    assertEquals(1, results.oFired)
  }

  @Test def shouldSOSWithSO(): Unit = {
    val buffer = StringBuilder.newBuilder
    val results = buildSO(Some(buffer))
    netWrapper.tickUntilCalm("1,0,0,0,1,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,0,0,0,1,0,0,0,1")
    assertEquals(6, results.dotFired)
    assertEquals(3, results.lineFired)
    assertEquals(2, results.sFired)
    assertEquals(1, results.oFired)

    assertEquals("SOS", buffer.toString())
  }

  private def buildSOSWithForwardHush(outputBuffer:Option[StringBuilder] = None) = {
    val dotBlockName = "Dot-"
    val dotExpectedDelay = 2
    val dotOutputId = DelayGate.outputId(dotBlockName)
    val dotHushId = DelayGate.hushId(dotBlockName)
    val dotInputId = DelayGate.inputId(dotBlockName)
    val dotMiddleId = DelayGate.middleId(dotBlockName)

    val lineBlockName = "Line-"
    val lineRequiredSignals = 2
    val lineOutputId = SignalSum.outputId(lineBlockName)
    val lineHushId = SignalSum.hushId(lineBlockName)
    val lineInputId = DelayGate.inputId(lineBlockName)

    val sBlockName = "S-"
    val sRequiredSignals = 3
    val sOutputId = SignalSum.outputId(sBlockName)
    val sHushId = SignalSum.hushId(sBlockName)
    val sInputId = DelayGate.inputId(sBlockName)

    val oBlockName = "O-"
    val oRequiredSignals = 3
    val oOutputId = SignalSum.outputId(oBlockName)
    val oHushId = SignalSum.hushId(oBlockName)
    val oInputId = DelayGate.inputId(oBlockName)

    super.build(
      NetBuilder().addInput("in")
        .delayGate(dotBlockName, dotExpectedDelay)
        .use("in").signalSum(lineBlockName, lineRequiredSignals)
        .use(dotOutputId).hush(lineHushId).use(lineOutputId).hush(dotHushId)
        .use(dotOutputId).signalSum(sBlockName, sRequiredSignals)
        .use(lineOutputId).signalSum(oBlockName, oRequiredSignals)
        .use(sOutputId).hush(oHushId).use(oOutputId).hush(sHushId)
        .use(lineOutputId).hush(sHushId)
        //.use(dotOutputId).hush(oHushId)
        .data
        // lines are more complex, so if we get a line, we can be sure it's not a dot
        // but if we get a dot, we cannot be entirely sure it's not really a line
        // so, with a line we can send a signal that the result is not S
        // but the other way around it's better not to do it
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

  @Test def shouldSOSWithForwardHush(): Unit = {
    val buffer = StringBuilder.newBuilder
    val results = buildSOSWithForwardHush(Some(buffer))
    netWrapper.tickUntilCalm("1,0,0,1,0,0,1,0,0,1,1,0,1,1,0,1,1,0,1,0,0,1,0,0,1,0,0")
    assertEquals("SOS", buffer.toString())
  }

  private def buildSOSWithAdditionalDelayGate(outputBuffer:Option[StringBuilder] = None) = {
    val dotBlockName = "Dot-"
    val dotExpectedDelay = 2
    val dotOutputId = DelayGate.outputId(dotBlockName)
    val dotHushId = DelayGate.hushId(dotBlockName)
    val dotInputId = DelayGate.inputId(dotBlockName)
    val dotMiddleId = DelayGate.middleId(dotBlockName)

    val dot2BlockName = "Dot2-"
    val dot2OutputId = DelayGate.outputId(dot2BlockName)
    val dot2HushId = DelayGate.hushId(dot2BlockName)
    val dot2InputId = DelayGate.inputId(dot2BlockName)
    val dot2MiddleId = DelayGate.middleId(dot2BlockName)

    val lineBlockName = "Line-"
    val lineRequiredSignals = 2
    val lineOutputId = SignalSum.outputId(lineBlockName)
    val lineHushId = SignalSum.hushId(lineBlockName)
    val lineInputId = DelayGate.inputId(lineBlockName)

    val sBlockName = "S-"
    val sRequiredSignals = 3
    val sOutputId = SignalSum.outputId(sBlockName)
    val sHushId = SignalSum.hushId(sBlockName)
    val sInputId = DelayGate.inputId(sBlockName)

    val oBlockName = "O-"
    val oRequiredSignals = 3
    val oOutputId = SignalSum.outputId(oBlockName)
    val oHushId = SignalSum.hushId(oBlockName)
    val oInputId = DelayGate.inputId(oBlockName)

    super.build(
      NetBuilder().addInput("in")
        .delayGate(dotBlockName, dotExpectedDelay)
        .use(dotOutputId).delayGate(dot2BlockName, 1)
        .use("in").constantCurrent(lineBlockName, lineRequiredSignals)
        .use(dot2OutputId).hush(lineHushId)
        .use(lineOutputId).hush(dotHushId).hush(dot2HushId)
        .use(dot2OutputId).signalSum(sBlockName, sRequiredSignals)
        .use(lineOutputId).signalSum(oBlockName, oRequiredSignals)
        .use(sOutputId).hush(oHushId).use(oOutputId).hush(sHushId)
        .data
    )

    LOG.allow(dotOutputId, dot2OutputId, lineOutputId, dotInputId, dot2InputId, lineInputId)

    val results = new TestResults;

    netWrapper.addAfterFire("in")((_:Double) =>{
      LOG.debug(s"${netWrapper.iteration}: Incoming!")
    })

    netWrapper.addAfterFire(dot2OutputId)((_:Double)=>{
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

  @Test def shouldSOSWithBreak(): Unit = {
    val buffer = StringBuilder.newBuilder
    val results = buildSOSWithAdditionalDelayGate(Some(buffer))
    netWrapper.tickUntilCalm("1,0,0,1,0,0,1,0,0,1,1,0,1,1,0,1,1,0,1,0,0,1,0,0,1,0,0")
    assertEquals("SOS", buffer.toString())
  }
}
