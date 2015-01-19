package test.async.epengine

import anna.async.NetBuilder
import anna.epengine.{Probability, NeuronChromosome, Engine, TossType}
import anna.logger.LOG
import anna.logger.LOG._
import anna.data._
import anna.utils.DoubleRange._
import org.junit.Assert._
import org.junit.{After, Before, Test}
import org.scalatest.junit.JUnitSuite

class EngineSuite extends JUnitSuite {
  private var engine: Engine = _

  @Before def before() {
    LOG.addLogToStdout()
    engine = new Engine()
    engine.synapseWeightRange = -1.0 <=> 1.0
    engine.synapseHushProbability = 0.1
    engine.synapsesTossType = TossType.LINEAR

    engine.thresholdRange = 0.0 <=> 0.9
    engine.slopeRange = 1.0 <=> 20.0
    engine.hushRange = 1 to 5
    engine.forgettingRange = 0.1 <=> 0.9
    engine.dontForgetProbability = 0.75
    engine.forgetAllProbability = 0.05
    engine.tickTimeMultiplierRange = 0.5 <=> 2.0
  }

  @After def after() {
  }

  @Test def shouldTossForSynapse() {
    val totalCount = 1000
    var hushCount = 0
    for (i <- 1 to totalCount) {
      val synapseChromosome = engine.tossForSynapse("id1")
      assertEquals("id1", synapseChromosome.neuronId)
      synapseChromosome.weight match {
        case Hush => hushCount = hushCount + 1
        case SynapseWeight(w) =>
          assert(engine.synapseWeightRange.contains(w), s"weight outside range: $w")
      }
    }

    debug(this, s"hushCount: $hushCount")
    assertTrue(hushCount > 80)
    assertTrue(hushCount < 120)
  }

  @Test def shouldChangeHushProbability() {
    engine.synapseHushProbability = 0.9

    val totalCount = 1000
    var hushCount = 0
    for (i <- 1 to totalCount) {
      val synapseChromosome = engine.tossForSynapse("id1")
      assertEquals("id1", synapseChromosome.neuronId)
      synapseChromosome.weight match {
        case Hush => hushCount = hushCount + 1
        case SynapseWeight(w) =>
          assert(engine.synapseWeightRange.contains(w), s"weight outside range: $w")
      }
    }

    debug(this, s"hushCount: $hushCount")
    assertTrue(hushCount > 800)
  }

  @Test def shouldTossForNeuron(): Unit = {
    engine.thresholdRange = 0.0 <=> 0.9
    engine.slopeRange = 1.0 <=> 20.0
    engine.hushRange = 1 to 5
    engine.forgettingRange = 0.1 <=> 0.9
    engine.dontForgetProbability = 0.75
    engine.forgetAllProbability = 0.05
    engine.tickTimeMultiplierRange = 0.5 <=> 2.0

    val totalCount = 1000
    var dontForgetCount = 0
    var forgetAllCount = 0
    for (i <- 1 to totalCount) {
      val neuronChromosome: NeuronChromosome = engine.tossForNeuron("id1")
      assertEquals("id1", neuronChromosome.id)
      assertTrue(engine.thresholdRange.contains(neuronChromosome.threshold))
      assertTrue(engine.slopeRange.contains(neuronChromosome.slope))
      assertTrue(engine.hushRange.contains(neuronChromosome.hushValue.iterations))
      neuronChromosome.forgetting match {
        case DontForget => dontForgetCount += 1
        case ForgetAll => forgetAllCount += 1
        case ForgetValue(value) => assertTrue(engine.forgettingRange.contains(value))
      }
    }
    assertTrue((600 to 800).contains(dontForgetCount))
    assertTrue((0 to 100).contains(forgetAllCount))
  }

  @Test def shouldTossForNet(): Unit = {
    engine.neuronsRange = 5 to 10
    engine.inputIds = List("in1")
    engine.outputIds = List("out1", "out2")
    engine.synapsesDensity = 2.5
    engine.inputTickMultiplierRange = 2.0 <=> 3.0

    val netChromosome = engine.tossForNet("net")
    assertEquals("net", netChromosome.id)
    assertTrue(netChromosome.neurons.size >= 5)
    assertTrue(netChromosome.neurons.size <= 10)

    val inOpt = netChromosome.find("in1")
    assertTrue(inOpt != None)
    val inN = inOpt.get
    assertTrue(inN.synapses.nonEmpty)

    val out1Opt = netChromosome.find("out1")
    assertTrue(out1Opt != None)
    val out1N = out1Opt.get
    assertTrue(out1N.synapses.isEmpty)

    val out2Opt = netChromosome.find("out2")
    assertTrue(out2Opt != None)
    val out2N = out2Opt.get
    assertTrue(out2N.synapses.isEmpty)

    assertTrue(netChromosome.inputTickMultiplier >= 2.0)
    assertTrue(netChromosome.inputTickMultiplier <= 3.0)

    val (in, net) = NetBuilder().set(netChromosome.net).build("in")
    in += "1,1,1"
    in.tickUntilCalm()
    debug(this, "iterations: " + in.iteration)
    assertTrue(in.iteration > 0)
  }

  @Test def shouldMutateSynapse(): Unit ={
    val sch = engine.tossForSynapse("id1")
    val originalWeight = sch.weight
    sch.mutate()
    val mutatedWeight = sch.weight
    assertNotEquals(originalWeight, mutatedWeight)
  }


  @Test def shouldMutateThreshold(): Unit ={
    val nch = engine.tossForNeuron("id1")
    val original = nch.threshold

    nch.thresholdProbability = 1.0
    nch.slopeProbability = 0.0
    nch.forgettingProbability = 0.0
    nch.hushProbability = 0.0
    nch.synapseChangeProbability = 0.0

    nch.mutate()

    val mutated = nch.threshold
    assertNotEquals(original, mutated)
  }

  @Test def shouldMutateSlope(): Unit ={
    val nch = engine.tossForNeuron("id1")
    val original = nch.slope

    nch.thresholdProbability = 0.0
    nch.slopeProbability = 1.0
    nch.forgettingProbability = 0.0
    nch.hushProbability = 0.0
    nch.synapseChangeProbability = 0.0

    nch.mutate()

    val mutated = nch.slope
    assertNotEquals(original, mutated)
  }


  @Test def shouldMutateForgetting(): Unit ={
    val nch = engine.tossForNeuron("id1")
    val original = nch.forgetting

    nch.thresholdProbability = 0.0
    nch.slopeProbability = 0.0
    nch.forgettingProbability = 1.0
    nch.hushProbability = 0.0
    nch.synapseChangeProbability = 0.0

    nch.forgetAllProbability = 1.0
    nch.dontForgetProbability = 0.0

    nch.mutate()

    var mutated = nch.forgetting
    assertEquals(ForgetAll, mutated)

    nch.forgetAllProbability = 0.0
    nch.dontForgetProbability = 1.0

    nch.mutate()

    mutated = nch.forgetting
    assertEquals(DontForget, mutated)

    nch.forgetAllProbability = 0.0
    nch.dontForgetProbability = 0.0

    nch.mutate()

    nch.forgetting match {
      case ForgetValue(value) => println("ok")
      case other => fail(s"The forgetting value should be some intermediate value, not $other")
    }
  }

  @Test def shouldMutateHush(){
    val nch = engine.tossForNeuron("id1")
    val original = nch.forgetting

    nch.thresholdProbability = 0.0
    nch.slopeProbability = 0.0
    nch.forgettingProbability = 1.0
    nch.hushProbability = 0.0
    nch.synapseChangeProbability = 0.0
  }

  @Test def shouldPerformRandom(): Unit ={
    var result = 0
    val f1 = () => { result = 1 }
    val f2 = () => { result = 2 }
    val f3 = () => { result = 3 }

    Probability.performRandom((1.0,f1), (0.0,f2), (0.0,f3))
    assertEquals(1, result)

    Probability.performRandom((0.0,f1), (1.0,f2), (0.0,f3))
    assertEquals(2, result)

    Probability.performRandom((0.0,f1), (0.0,f2), (1.0,f3))
    assertEquals(3, result)

    Probability.performRandom((0.0,f1), (0.0,f2), (0.0,f3))
    assertEquals(1, result)
  }
}