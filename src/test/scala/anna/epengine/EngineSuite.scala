package test.async.epengine

import anna.async.NetBuilder
import anna.epengine._
import anna.logger.LOG
import anna.logger.LOG._
import anna.data._
import anna.utils.DoubleRange._
import org.junit.Assert._
import org.junit.{After, Before, Test}
import org.scalatest.junit.JUnitSuite

class EngineSuite extends JUnitSuite {

  @Before def before() {
    LOG.addLogToStdout()

    SynapseGenome.weightRange = -1.0 <=> 1.0
    SynapseGenome.hushProbability = 0.1

    NeuronGenome.thresholdRange = 0.0 <=> 0.9
    NeuronGenome.slopeRange = 1.0 <=> 20.0
    NeuronGenome.hushRange = 1 to 5
    NeuronGenome.forgettingRange = 0.1 <=> 0.9
    NeuronGenome.dontForgetProbability = 0.75
    NeuronGenome.forgetAllProbability = 0.05
    NeuronGenome.tickTimeMultiplierRange = 0.5 <=> 2.0
  }

  @After def after() {
  }

  @Test def shouldTossForSynapse() {
    val totalCount = 1000
    var hushCount = 0
    for (i <- 1 to totalCount) {
      val sg = SynapseGenome.toss("id1")
      assertEquals("id1", sg.neuronId)
      sg.weight match {
        case Hush => hushCount = hushCount + 1
        case SynapseWeight(w) =>
          assert(SynapseGenome.weightRange.contains(w), s"weight outside range: $w")
      }
    }

    debug(this, s"hushCount: $hushCount")
    assertTrue(hushCount > 70)
    assertTrue(hushCount < 130)
  }

  @Test def shouldChangeHushProbability() {
    SynapseGenome.hushProbability = 1.0
    SynapseGenome.fullWeightProbability = 0.0

    val totalCount = 1000
    var hushCount = 0
    for (i <- 1 to totalCount) {
      val sg = SynapseGenome.toss("id1")
      assertEquals("id1", sg.neuronId)
      sg.weight match {
        case Hush => hushCount = hushCount + 1
        case SynapseWeight(w) =>
          assert(SynapseGenome.weightRange.contains(w), s"weight outside range: $w")
      }
    }

    debug(this, s"hushCount: $hushCount")
    assertTrue(hushCount > 800)
  }

  @Test def shouldTossForNeuron(): Unit = {
    NeuronGenome.thresholdRange = 0.0 <=> 0.9
    NeuronGenome.slopeRange = 1.0 <=> 20.0
    NeuronGenome.hushRange = 1 to 5
    NeuronGenome.forgettingRange = 0.1 <=> 0.9
    NeuronGenome.dontForgetProbability = 0.75
    NeuronGenome.forgetAllProbability = 0.05
    NeuronGenome.tickTimeMultiplierRange = 0.5 <=> 2.0

    val totalCount = 1000
    var dontForgetCount = 0
    var forgetAllCount = 0
    for (i <- 1 to totalCount) {
      val ng = NeuronGenome.toss("id1")
      assertEquals("id1", ng.id)
      assertTrue(NeuronGenome.thresholdRange.contains(ng.threshold))
      assertTrue(NeuronGenome.slopeRange.contains(ng.slope))
      assertTrue(NeuronGenome.hushRange.contains(ng.hushValue.iterations))
      ng.forgetting match {
        case DontForget => dontForgetCount += 1
        case ForgetAll => forgetAllCount += 1
        case ForgetValue(value) => assertTrue(NeuronGenome.forgettingRange.contains(value))
      }
    }
    assertTrue((600 to 800).contains(dontForgetCount))
    assertTrue((0 to 100).contains(forgetAllCount))
  }

  @Test def shouldTossForNet(): Unit = {
    NetGenome.neuronsRange = 5 to 10
    NetGenome.synapsesDensity = 2.5
    NetGenome.inputTickMultiplierRange = 2.0 <=> 3.0

    val inputIds = List("in1")
    val outputIds = List("out1", "out2")

    val ng = NetGenome.toss("net",inputIds,outputIds)
    assertEquals("net", ng.id)
    assertTrue(ng.neurons.size >= 5)
    assertTrue(ng.neurons.size <= 10)

    val inOpt = ng.find("in1")
    assertTrue(inOpt != None)
    val inN = inOpt.get
    assertTrue(inN.synapses.nonEmpty)

    val out1Opt = ng.find("out1")
    assertTrue(out1Opt != None)
    val out1N = out1Opt.get
    assertTrue(out1N.synapses.isEmpty)

    val out2Opt = ng.find("out2")
    assertTrue(out2Opt != None)
    val out2N = out2Opt.get
    assertTrue(out2N.synapses.isEmpty)

    assertTrue(ng.inputTickMultiplier >= 2.0)
    assertTrue(ng.inputTickMultiplier <= 3.0)

    val (in, net) = NetBuilder().set(ng.data).build("in")
    in += "1,1,1"
    in.tickUntilCalm()
    debug(this, "iterations: " + in.iteration)
    assertTrue(in.iteration > 0)
  }

  @Test def shouldMutateSynapse(): Unit ={
    val sg = SynapseGenome.toss("id1")
    val originalWeight = sg.weight
    sg.mutate()
    val mutatedWeight = sg.weight
    assertNotEquals(originalWeight, mutatedWeight)
  }


  @Test def shouldMutateThreshold(): Unit ={
    val ng = NeuronGenome.toss("id1")
    val original = ng.threshold

    NeuronGenome.thresholdProbability = 1.0
    NeuronGenome.slopeProbability = 0.0
    NeuronGenome.forgettingProbability = 0.0
    NeuronGenome.hushProbability = 0.0
    NeuronGenome.synapseChangeProbability = 0.0

    ng.mutate()

    val mutated = ng.threshold
    assertNotEquals(original, mutated)
  }

  @Test def shouldMutateSlope(): Unit ={
    val ng = NeuronGenome.toss("id1")
    val original = ng.slope

    NeuronGenome.thresholdProbability = 0.0
    NeuronGenome.slopeProbability = 1.0
    NeuronGenome.forgettingProbability = 0.0
    NeuronGenome.hushProbability = 0.0
    NeuronGenome.synapseChangeProbability = 0.0

    ng.mutate()

    val mutated = ng.slope
    assertNotEquals(original, mutated)
  }


  @Test def shouldMutateForgetting(): Unit ={
    val ng = NeuronGenome.toss("id1")
    val original = ng.forgetting

    NeuronGenome.thresholdProbability = 0.0
    NeuronGenome.slopeProbability = 0.0
    NeuronGenome.forgettingProbability = 1.0
    NeuronGenome.hushProbability = 0.0
    NeuronGenome.synapseChangeProbability = 0.0
    NeuronGenome.tickTimeMultiplierProbability = 0.0

    NeuronGenome.forgetAllProbability = 1.0
    NeuronGenome.dontForgetProbability = 0.0

    ng.mutate()

    var mutated = ng.forgetting
    assertEquals(ForgetAll, mutated)

    NeuronGenome.forgetAllProbability = 0.0
    NeuronGenome.dontForgetProbability = 1.0

    ng.mutate()

    mutated = ng.forgetting
    assertEquals(DontForget, mutated)

    NeuronGenome.forgetAllProbability = 0.0
    NeuronGenome.dontForgetProbability = 0.0

    ng.mutate()

    ng.forgetting match {
      case ForgetValue(value) => println("ok")
      case other => fail(s"The forgetting value should be some intermediate value, not $other")
    }
  }

  @Test def shouldMutateHush(){
    NeuronGenome.hushRange = 1 to 1
    val ng = NeuronGenome.toss("id1")
    val original = ng.hushValue
    assertEquals(HushValue(1), ng.hushValue)

    NeuronGenome.thresholdProbability = 0.0
    NeuronGenome.slopeProbability = 0.0
    NeuronGenome.forgettingProbability = 0.0
    NeuronGenome.hushProbability = 1.0
    NeuronGenome.synapseChangeProbability = 0.0
    NeuronGenome.tickTimeMultiplierProbability = 0.0

    NeuronGenome.hushRange = 2 to 5

    ng.mutate()
    val mutated = ng.hushValue
    assertNotEquals(original, mutated)
    assertTrue(NeuronGenome.hushRange.contains(mutated.iterations))
  }

  @Test def shouldAddSynapse(): Unit ={
    val idSet = Set("id1","id2")
    val accessMap = Map("id1" -> MutationAccess.FULL, "id2" -> MutationAccess.FULL)
    val ng1 = NeuronGenome.toss("id1", accessMap)
    val ng2 = NeuronGenome.toss("id2", accessMap)

    assertEquals(0, ng1.synapses.size)

    NeuronGenome.thresholdProbability = 0.0
    NeuronGenome.slopeProbability = 0.0
    NeuronGenome.forgettingProbability = 0.0
    NeuronGenome.hushProbability = 0.0
    NeuronGenome.synapseChangeProbability = 1.0
    NeuronGenome.tickTimeMultiplierProbability = 0.0

    NeuronGenome.addSynapseProbability = 1.0
    NeuronGenome.deleteSynapseProbability = 0.0

    ng1.mutate()

    assertEquals(1, ng1.synapses.size)
    val s = ng1.synapses(0)
    assertTrue(idSet.contains(s.neuronId))

    // a mutation should not result in adding a second synapse pointing to an already connected neuron
    // so if we mutate this neuron again, in 100% cases it should result in connections both to id1 and id2
    ng1.mutate()

    assertEquals(2, ng1.synapses.size)
    assertEquals(idSet, ng1.synapses.map(_.neuronId).toSet)

    val oldWayOfDoingCopy = NeuronGenome(ng1.data, accessMap)
    val clone = ng1.clone
    assertEquals(oldWayOfDoingCopy.data, clone.data)
    assertEquals(oldWayOfDoingCopy.accessMap, clone.accessMap)

    // nothing should change as there is no way to add another synapse
    ng1.mutate()

    assertEquals(clone.data, ng1.data)
  }

  @Test def shoulDeleteSynapse(): Unit ={
    val idSet = Set("id1","id2")
    val accessMap = Map("id1" -> MutationAccess.FULL, "id2" -> MutationAccess.FULL)
    val ng1 = NeuronGenome.toss("id1", accessMap)
    val ng2 = NeuronGenome.toss("id2", accessMap)

    ng1.addSynapse(SynapseGenome("id2",Hush))

    assertEquals(1, ng1.synapses.size)

    NeuronGenome.thresholdProbability = 0.0
    NeuronGenome.slopeProbability = 0.0
    NeuronGenome.forgettingProbability = 0.0
    NeuronGenome.hushProbability = 0.0
    NeuronGenome.synapseChangeProbability = 1.0
    NeuronGenome.tickTimeMultiplierProbability = 0.0

    NeuronGenome.addSynapseProbability = 0.0
    NeuronGenome.deleteSynapseProbability = 1.0

    ng1.mutate()
    assertEquals(0, ng1.synapses.size)

    val clone = ng1.clone

    // nothing should change as there is no more synapses to remove
    ng1.mutate()
    assertEquals(0, ng1.synapses.size)
    assertEquals(clone.data, ng1.data)
  }

  @Test def shouldChangeSynapseWeight(): Unit ={
    val idSet = Set("id1","id2")
    val accessMap = Map("id1" -> MutationAccess.FULL, "id2" -> MutationAccess.FULL)
    val nch1 = NeuronGenome.toss("id1", accessMap)
    val nch2 = NeuronGenome.toss("id2", accessMap)

    nch1.addSynapse(SynapseGenome("id2",Hush))

    assertEquals(1, nch1.synapses.size)

    NeuronGenome.thresholdProbability = 0.0
    NeuronGenome.slopeProbability = 0.0
    NeuronGenome.forgettingProbability = 0.0
    NeuronGenome.hushProbability = 0.0
    NeuronGenome.synapseChangeProbability = 1.0
    NeuronGenome.tickTimeMultiplierProbability = 0.0

    NeuronGenome.addSynapseProbability = 0.0
    NeuronGenome.deleteSynapseProbability = 0.0

    SynapseGenome.fullWeightProbability = 1.0
    SynapseGenome.hushProbability = 0.0

    nch1.mutate()
    assertEquals(SynapseWeight(1.0), nch1.getSynapse("id2").weight)

    SynapseGenome.fullWeightProbability = 0.0
    SynapseGenome.hushProbability = 1.0

    nch1.mutate()
    assertEquals(Hush, nch1.getSynapse("id2").weight)

    SynapseGenome.fullWeightProbability = 0.0
    SynapseGenome.hushProbability = 0.0

    nch1.mutate()
    val weight = nch1.getSynapse("id2").weight
    assertNotEquals(Hush, weight)
    assertNotEquals(SynapseWeight(1.0), weight)
  }

  @Test def shouldAddNeuron(): Unit ={
    NetGenome.neuronsRange = 2 to 2
    NetGenome.synapsesDensity = 2.5
    NetGenome.inputTickMultiplierRange = 2.0 <=> 2.0

    val inputIds = List("in1")
    val outputIds = List("out1")

    val ng = NetGenome.toss("net", inputIds, outputIds)
    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in1"))
    assertNotEquals(None, ng.find("out1"))

    NetGenome.addNeuronProbability = 1.0
    NetGenome.deleteNeuronProbability = 0.0
    NetGenome.mutateNeuronProbability = 0.0
    NetGenome.inputTickMultiplierProbability = 0.0

    ng.mutate()

    val middle1Opt = ng.find("net2")
    assertNotEquals(None, middle1Opt)
    assertTrue(ng.findSynapse("in1","net2") != None || ng.findSynapse("out1","net2") != None)
    assertNotEquals(None, ng.findSynapse("net2","out1"))
  }

  @Test def shouldDeleteNeuron(): Unit ={
    NetGenome.neuronsRange = 3 to 3
    NetGenome.synapsesDensity = 2.5
    NetGenome.inputTickMultiplierRange = 2.0 <=> 2.0

    val inputIds = List("in1")
    val outputIds = List("out1")

    val ng = NetGenome.toss("net", inputIds, outputIds)
    assertEquals(3, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in1"))
    assertNotEquals(None, ng.find("out1"))

    NetGenome.addNeuronProbability = 0.0
    NetGenome.deleteNeuronProbability = 1.0
    NetGenome.mutateNeuronProbability = 0.0
    NetGenome.inputTickMultiplierProbability = 0.0

    ng.mutate()

    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in1"))
    assertNotEquals(None, ng.find("out1"))
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