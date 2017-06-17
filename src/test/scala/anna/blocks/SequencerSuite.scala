package anna.blocks

import anna.async.{NetBuilder, NetWrapper}
import anna.async.NetBuilderOps._
import anna.logger.LOG
import org.junit.Assert._
import org.junit.{After, Test}
import org.scalatest.junit.JUnitSuite

/**
  * Created by samuel on 17.06.17.
  */
class SequencerSuite extends JUnitSuite {
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

  @Test def shouldAccept1001(): Unit = {
    sequencer("sequencer")

    var fired = false
    netWrapper.addAfterFire("sequencer2"){ fired = true }
    netWrapper.iterateUntilCalm("10,01")

    assertTrue(fired)
  }

  @Test def shouldIgnore10(): Unit = {
    sequencer("sequencer")
    var fired = false

    netWrapper.addAfterFire("sequencer2"){ fired = true }
    netWrapper.iterateUntilCalm("10")

    assertFalse(fired)
  }

  @Test def shouldIgnore01(): Unit = {
    sequencer("sequencer")
    var fired = false

    netWrapper.addAfterFire("sequencer2"){ fired = true }
    netWrapper.iterateUntilCalm("01")

    assertFalse(fired)
  }

  @Test def shouldIgnore1010(): Unit = {
    sequencer("sequencer")
    var fired = false

    netWrapper.addAfterFire("sequencer2"){ fired = true }
    netWrapper.iterateUntilCalm("10,10")

    assertFalse(fired)
  }


  @Test def shouldIgnore0101(): Unit = {
    sequencer("sequencer")
    var fired = false

    netWrapper.addAfterFire("sequencer2"){ fired = true }
    netWrapper.iterateUntilCalm("10,10")

    assertFalse(fired)
  }


  @Test def shouldAcceptTwoConsecutive(): Unit = {
    sequencer("sequencer")
    var fired = 0

    netWrapper.addAfterFire("sequencer2"){ fired += 1 }
    netWrapper.iterateUntilCalm("10,01,10,01")

    assertEquals(2, fired)
  }

  @Test def shouldAcceptOnlyFirst1(): Unit = {
    sequencer("sequencer")
    var fired = 0

    netWrapper.addAfterFire("sequencer2"){ fired += 1 }
    netWrapper.iterateUntilCalm("10,01,10,10")

    assertEquals(1, fired)
  }

  @Test def shouldAcceptOnlyFirst2(): Unit = {
    sequencer("sequencer")
    var fired = 0

    netWrapper.addAfterFire("sequencer2"){ fired += 1 }
    netWrapper.iterateUntilCalm("10,01,01,01")

    assertEquals(1, fired)
  }

  @Test def shouldAcceptOnlySecond1(): Unit = {
    sequencer("sequencer")
    var fired = 0

    netWrapper.addAfterFire("sequencer2"){ fired += 1 }
    netWrapper.iterateUntilCalm("10,10,10,01")

    assertEquals(1, fired)
  }

  @Test def shouldAcceptOnlySecond2(): Unit = {
    sequencer("sequencer")
    var fired = 0

    netWrapper.addAfterFire("sequencer2"){ fired += 1 }
    netWrapper.iterateUntilCalm("01,01,10,01")

    assertEquals(1, fired)
  }

  @Test def shouldReset(): Unit = {
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
