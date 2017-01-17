package anna.blocks

import anna.async.{NetBuilder, NetWrapper}
import org.junit.{After, Before, Test}
import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import anna.async.NetBuilderOps._


/**
  * Created by gorywoda on 1/17/17.
  */
class SequencerSuite extends JUnitSuite  {

  private var netWrapper:NetWrapper = _

  val IN1 = "in1"
  val IN2 = "in2"
  val SEQ = "seq"

  @Before def setUp(): Unit ={
    netWrapper = NetBuilder().addInput(IN1).sequencer(SEQ).addInput(IN2).sequencer(SEQ).build()
  }
  
  @After def tearDown(): Unit ={
    netWrapper.shutdown()
  }
  
  @Test def shouldBuildSequencerWithTwoInputsAndOneOutput(): Unit ={
    val inputIds = netWrapper.inputIds
    // ensuring they are in sequence - that's important here (although it should be tested elsewhere)
    assertEquals(2, inputIds.size)
    assertEquals(IN1, inputIds(0))
    assertEquals(IN2, inputIds(1))

    val neuronIds = netWrapper.neuronIds.toSet
    assertTrue(neuronIds.contains(Sequencer.inputId1(SEQ)))
    assertTrue(neuronIds.contains(Sequencer.inputId2(SEQ)))
    assertTrue(neuronIds.contains(Sequencer.outputId(SEQ)))
  }

  @Test def shouldSignalAfterBothInputs(): Unit ={
    val outId = Sequencer.outputId(SEQ)
    var fired = false
    netWrapper.addAfterFire(outId){ fired = true }

    netWrapper.iterateUntilCalm("10,01")

    assertTrue(fired)
  }

  @Test def shouldNotSignalAfterFirstInputOnly(): Unit ={
    val outId = Sequencer.outputId(SEQ)
    var fired = false
    netWrapper.addAfterFire(outId){ fired = true }

    netWrapper.iterateUntilCalm("10")

    assertFalse(fired)
  }

  @Test def shouldNotSignalAfterSecondInputOnly(): Unit ={
    val outId = Sequencer.outputId(SEQ)
    var fired = false
    netWrapper.addAfterFire(outId){ fired = true }

    netWrapper.iterateUntilCalm("01")

    assertFalse(fired)
  }

  @Test def shouldNotSignalAfterManyFirstInputs(): Unit ={
    val outId = Sequencer.outputId(SEQ)
    var fired = false
    netWrapper.addAfterFire(outId){ fired = true }

    netWrapper.iterateUntilCalm("10,10,10")

    assertFalse(fired)
  }

  @Test def shouldNotSignalAfterManySecondInputs(): Unit ={
    val outId = Sequencer.outputId(SEQ)
    var fired = false
    netWrapper.addAfterFire(outId){ fired = true }

    netWrapper.iterateUntilCalm("01,01,01")

    assertFalse(fired)
  }
}
