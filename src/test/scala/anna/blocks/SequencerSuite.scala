package anna.blocks

import anna.async.{NetBuilder, NetWrapper}
import anna.async.NetBuilderOps._
import anna.logger.LOG
import org.junit.Assert._
import org.junit.{After, Test}
import org.scalatest.funsuite.AnyFunSuite

class SequencerSuite extends AnyFunSuite {
  private var _wrapper: Option[NetWrapper] = None

  @After def tearDown(): Unit = _wrapper match {
    case Some(wrapper) => wrapper.shutdown(); _wrapper = None
    case None =>
  }

  private def set(wrapper: NetWrapper) = _wrapper = Some(wrapper)

  private def netWrapper = _wrapper.get

  private def sequencer(blockName: String) =
    set(NetBuilder().addInput("in1").addInput("in2").sequencer(blockName, "in1", "in2").build())

  private def sequencerSilencer(blockName: String) = set(
    NetBuilder().addInput("in1").addInput("in2").sequencer(blockName, "in1", "in2")
      .addInput("in3").silence("sequencers")
      .build()
  )

  test("shouldAccept1001(") {
    sequencer("sequencer")

    var fired = false
    netWrapper.addAfterFire("sequencer2"){ fired = true }
    netWrapper.iterateUntilCalm("10,01")

    assertTrue(fired)
  }

  test("shouldIgnore10(") {
    sequencer("sequencer")
    var fired = false

    netWrapper.addAfterFire("sequencer2"){ fired = true }
    netWrapper.iterateUntilCalm("10")

    assertFalse(fired)
  }

  test("shouldIgnore01(") {
    sequencer("sequencer")
    var fired = false

    netWrapper.addAfterFire("sequencer2"){ fired = true }
    netWrapper.iterateUntilCalm("01")

    assertFalse(fired)
  }

  test("shouldIgnore1010(") {
    sequencer("sequencer")
    var fired = false

    netWrapper.addAfterFire("sequencer2"){ fired = true }
    netWrapper.iterateUntilCalm("10,10")

    assertFalse(fired)
  }


  test("shouldIgnore0101(") {
    sequencer("sequencer")
    var fired = false

    netWrapper.addAfterFire("sequencer2"){ fired = true }
    netWrapper.iterateUntilCalm("10,10")

    assertFalse(fired)
  }


  test("shouldAcceptTwoConsecutive(") {
    sequencer("sequencer")
    var fired = 0

    netWrapper.addAfterFire("sequencer2"){ fired += 1 }
    netWrapper.iterateUntilCalm("10,01,10,01")

    assertEquals(2, fired)
  }

  test("shouldAcceptOnlyFirst1(") {
    sequencer("sequencer")
    var fired = 0

    netWrapper.addAfterFire("sequencer2"){ fired += 1 }
    netWrapper.iterateUntilCalm("10,01,10,10")

    assertEquals(1, fired)
  }

  test("shouldAcceptOnlyFirst2(") {
    sequencer("sequencer")
    var fired = 0

    netWrapper.addAfterFire("sequencer2"){ fired += 1 }
    netWrapper.iterateUntilCalm("10,01,01,01")

    assertEquals(1, fired)
  }

  test("shouldAcceptOnlySecond1(") {
    sequencer("sequencer")
    var fired = 0

    netWrapper.addAfterFire("sequencer2"){ fired += 1 }
    netWrapper.iterateUntilCalm("10,10,10,01")

    assertEquals(1, fired)
  }

  test("shouldAcceptOnlySecond2(") {
    sequencer("sequencer")
    var fired = 0

    netWrapper.addAfterFire("sequencer2"){ fired += 1 }
    netWrapper.iterateUntilCalm("01,01,10,01")

    assertEquals(1, fired)
  }

  test("shouldReset(") {
    sequencerSilencer("sequencer")
    var fired = 0

    netWrapper.addAfterFire("sequencer2"){ fired += 1 }
    netWrapper.iterateUntilCalm("100,010")
    assertEquals(1, fired)
    netWrapper.iterateUntilCalm("100,001,010")
    assertEquals(1, fired)
    netWrapper.iterateUntilCalm("100,001,100,010")
    assertEquals(2, fired)
  }
}
