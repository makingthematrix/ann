package anna.blocks

import anna.async.NetBuilder
import anna.async.NetBuilderOps._
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

/**
  * Created by gorywoda on 7/2/16.
  */
class SOSWithBlocksSuite extends JUnitSuite {

  private def buildDot = {
    val dotBlockName = "DOT"
    val expectedDelay = 2
    val outputId = DelayGate.outputId(dotBlockName)

    val netWrapper = NetBuilder().addInput("in").delayGate(dotBlockName, expectedDelay).build()
    assertNotNull(netWrapper.find(outputId))

    (netWrapper, outputId)
  }

  @Test def shouldFireOnDot(): Unit = {
    var fired = false
    var iteration = 0
    val (netWrapper, outputId) = buildDot

    netWrapper.addAfterFire(outputId){
      iteration = netWrapper.iteration
      fired = true
    }

    netWrapper.iterateUntilCalm("1")

    assertTrue(fired)
    assertEquals(2, iteration-1)

    netWrapper.shutdown()
  }

  private def buildLine = {
    val lineBlockName = "LINE"
    val requiredSignals = 2
    val outputId = SignalSum.outputId(lineBlockName)

    val netWrapper = NetBuilder().addInput("in").signalSum(lineBlockName, 2).build()
    assertNotNull(netWrapper.find(outputId))

    (netWrapper, outputId)
  }

  @Test def shouldFireOnLine(): Unit = {
    var fired = false

    val (netWrapper, outputId) = buildLine

    netWrapper.addAfterFire(outputId){ fired = true }

    LOG.allow(outputId)

    netWrapper.iterateUntilCalm("1,1")
    assertTrue(fired)

    fired = false
    netWrapper.iterateUntilCalm("1")
    assertFalse(fired)

    netWrapper.shutdown()
  }

  private class TestResults {
    var dotFired = 0
    var lineFired = 0
    var sFired = 0
    var oFired = 0
  }

  private def buildDotLine = {
    val dotBlockName = "DOT"
    val expectedDelay = 2
    val dotOutputId = DelayGate.outputId(dotBlockName)
    val dotSilencingId = DelayGate.silencingId(dotBlockName)
    val dotInputId = DelayGate.inputId(dotBlockName)
    val dotMiddleId = DelayGate.middleId(dotBlockName)

    val lineBlockName = "LINE"
    val requiredSignals = 2
    val lineOutputId = SignalSum.outputId(lineBlockName)
    val lineSilencingId = SignalSum.silencingId(lineBlockName)
    val lineInputId = SignalSum.inputId(lineBlockName)

    val netWrapper =
      NetBuilder().addInput("in")
        .delayGate(dotBlockName, expectedDelay)
        .use("in").signalSum(lineBlockName, requiredSignals)
        .use(dotOutputId).silence(lineSilencingId)
        .use(lineOutputId).silence(dotSilencingId)
        .build()

    LOG.allow(dotOutputId, lineOutputId, dotSilencingId, dotInputId, dotMiddleId)

    val results = new TestResults;

    netWrapper.addAfterFire("in"){
      LOG.debug(s"${netWrapper.iteration}: Incoming!")
    }

    netWrapper.addAfterFire(dotOutputId){
      LOG.debug(s"${netWrapper.iteration}: Dot!")
      results.dotFired += 1
    }

    netWrapper.addAfterFire(lineOutputId){
      LOG.debug(s"${netWrapper.iteration}: Line!")
      results.lineFired += 1
    }

    (netWrapper, results)
  }

  @Test def shouldDotWithDotLine(): Unit = {
    val (netWrapper, results) = buildDotLine

    netWrapper.iterateUntilCalm("1")

    assertEquals(1, results.dotFired)
    assertEquals(0, results.lineFired)

    netWrapper.shutdown()
  }

  @Test def shouldLineWithDotLine(): Unit = {
    val (netWrapper, results) = buildDotLine

    netWrapper.iterateUntilCalm("1,1")

    assertEquals(0, results.dotFired)
    assertEquals(1, results.lineFired)

    netWrapper.shutdown()
  }

  @Test def shouldTwoDotsWithDotLine(): Unit = {
    val (netWrapper, results) = buildDotLine

    netWrapper.iterateUntilCalm("1,0,0,0,1")

    assertEquals(2, results.dotFired)
    assertEquals(0, results.lineFired)

    netWrapper.shutdown()
  }

  @Test def shouldLineAndDotWithDotLine(): Unit = {
    val (netWrapper, results) = buildDotLine

    netWrapper.iterateUntilCalm("1,1,0,0,1")

    assertEquals(1, results.dotFired)
    assertEquals(1, results.lineFired)

    netWrapper.shutdown()
  }

  @Test def shouldDotAndLineWithDotLine(): Unit = {
    val (netWrapper, results) = buildDotLine

    netWrapper.iterateUntilCalm("1,0,0,0,1,1")

    assertEquals(1, results.dotFired)
    assertEquals(1, results.lineFired)

    netWrapper.shutdown()
  }

  @Test def shouldThreeDotsWithDotLine(): Unit = {
    val (netWrapper, results) = buildDotLine

    netWrapper.iterateUntilCalm("1,0,0,0,1,0,0,0,1")

    assertEquals(3, results.dotFired)
    assertEquals(0, results.lineFired)

    netWrapper.shutdown()
  }

  @Test def shouldThreeLinesWithDotLine(): Unit = {
    val (netWrapper, results) = buildDotLine

    netWrapper.iterateUntilCalm("1,1,0,0,1,1,0,0,1,1")

    assertEquals(0, results.dotFired)
    assertEquals(3, results.lineFired)

    netWrapper.shutdown()
  }

  private def buildSO(outputBuffer:Option[StringBuilder] = None) = {
    val dotBlockName = "DOT"
    val dotExpectedDelay = 2
    val dotOutputId = DelayGate.outputId(dotBlockName)
    val dotHushId = DelayGate.silencingId(dotBlockName)
    val dotInputId = DelayGate.inputId(dotBlockName)
    val dotMiddleId = DelayGate.middleId(dotBlockName)

    val lineBlockName = "LINE"
    val lineRequiredSignals = 2
    val lineOutputId = SignalSum.outputId(lineBlockName)
    val lineHushId = SignalSum.silencingId(lineBlockName)
    val lineInputId = SignalSum.inputId(lineBlockName)

    val sBlockName = "S"
    val sRequiredSignals = 3
    val sOutputId = SignalSum.outputId(sBlockName)
    val sHushId = SignalSum.silencingId(sBlockName)
    val sInputId = SignalSum.inputId(sBlockName)

    val oBlockName = "O"
    val oRequiredSignals = 3
    val oOutputId = SignalSum.outputId(oBlockName)
    val oHushId = SignalSum.silencingId(oBlockName)
    val oInputId = SignalSum.inputId(oBlockName)

    val netWrapper =
      NetBuilder().addInput("in")
        .delayGate(dotBlockName, dotExpectedDelay)
        .use("in").signalSum(lineBlockName, lineRequiredSignals)
        .use(dotOutputId).silence(lineHushId)
        .use(lineOutputId).silence(dotHushId)
        .use(dotOutputId).signalSum(sBlockName, sRequiredSignals)
        .use(lineOutputId).signalSum(oBlockName, oRequiredSignals)
        .use(sOutputId).silence(oHushId)
        .use(oOutputId).silence(sHushId)
        .build()


    LOG.allow(dotOutputId, lineOutputId, dotHushId, lineHushId, dotInputId, lineInputId, dotMiddleId)

    val results = new TestResults

    netWrapper.addAfterFire("in"){
      LOG.debug(s"${netWrapper.iteration}: Incoming!")
    }

    netWrapper.addAfterFire(dotOutputId){
      LOG.debug(s"${netWrapper.iteration}: Dot!")
      results.dotFired += 1
    }

    netWrapper.addAfterFire(lineOutputId){
      LOG.debug(s"${netWrapper.iteration}: Line!")
      results.lineFired += 1
    }

    netWrapper.addAfterFire(sOutputId){
      LOG.debug(s"${netWrapper.iteration}: S!")
      results.sFired += 1
      outputBuffer match {
        case Some(buffer) => buffer.append('S')
        case None =>
      }
    }

    netWrapper.addAfterFire(oOutputId){
      LOG.debug(s"${netWrapper.iteration}: O!")
      results.oFired += 1
      outputBuffer match {
        case Some(buffer) => buffer.append('O')
        case None =>
      }
    }

    (netWrapper, results)
  }

  @Test def shouldSWithSO(): Unit = {
    val (netWrapper, results) = buildSO()

    netWrapper.iterateUntilCalm("1,0,0,0,1,0,0,0,1")

    assertEquals(3, results.dotFired)
    assertEquals(0, results.lineFired)
    assertEquals(1, results.sFired)
    assertEquals(0, results.oFired)

    netWrapper.shutdown()
  }

  @Test def shouldOWithSO(): Unit = {
    val (netWrapper, results) = buildSO()

    netWrapper.iterateUntilCalm("1,1,0,0,1,1,0,0,1,1")

    assertEquals(0, results.dotFired)
    assertEquals(3, results.lineFired)
    assertEquals(0, results.sFired)
    assertEquals(1, results.oFired)

    netWrapper.shutdown()
  }

  @Test def shouldSOWithSO(): Unit = {
    val (netWrapper, results) = buildSO()

    netWrapper.iterateUntilCalm("1,0,0,0,1,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1")

    assertEquals(3, results.dotFired)
    assertEquals(3, results.lineFired)
    assertEquals(1, results.sFired)
    assertEquals(1, results.oFired)

    netWrapper.shutdown()
  }

  /* This is the test which proves it all works :) */

  @Test def shouldSOSWithSO(): Unit = {
    val buffer = StringBuilder.newBuilder
    val (netWrapper, results) = buildSO(Some(buffer))

    netWrapper.iterateUntilCalm("1,0,0,0,1,0,0,0,1,0,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,0,0,0,1,0,0,0,1")

    assertEquals(6, results.dotFired)
    assertEquals(3, results.lineFired)
    assertEquals(2, results.sFired)
    assertEquals(1, results.oFired)

    assertEquals("SOS", buffer.toString())

    netWrapper.shutdown()
  }

}
