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
      val neuronChromosome: NeuronGenome = engine.tossForNeuron("id1")
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

    val (in, net) = NetBuilder().set(netChromosome.data).build("in")
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

    NeuronGenome.thresholdProbability = 1.0
    NeuronGenome.slopeProbability = 0.0
    NeuronGenome.forgettingProbability = 0.0
    NeuronGenome.hushProbability = 0.0
    NeuronGenome.synapseChangeProbability = 0.0

    nch.mutate()

    val mutated = nch.threshold
    assertNotEquals(original, mutated)
  }

  @Test def shouldMutateSlope(): Unit ={
    val nch = engine.tossForNeuron("id1")
    val original = nch.slope

    NeuronGenome.thresholdProbability = 0.0
    NeuronGenome.slopeProbability = 1.0
    NeuronGenome.forgettingProbability = 0.0
    NeuronGenome.hushProbability = 0.0
    NeuronGenome.synapseChangeProbability = 0.0

    nch.mutate()

    val mutated = nch.slope
    assertNotEquals(original, mutated)
  }


  @Test def shouldMutateForgetting(): Unit ={
    val nch = engine.tossForNeuron("id1")
    val original = nch.forgetting

    NeuronGenome.thresholdProbability = 0.0
    NeuronGenome.slopeProbability = 0.0
    NeuronGenome.forgettingProbability = 1.0
    NeuronGenome.hushProbability = 0.0
    NeuronGenome.synapseChangeProbability = 0.0

    NeuronGenome.forgetAllProbability = 1.0
    NeuronGenome.dontForgetProbability = 0.0

    nch.mutate()

    var mutated = nch.forgetting
    assertEquals(ForgetAll, mutated)

    NeuronGenome.forgetAllProbability = 0.0
    NeuronGenome.dontForgetProbability = 1.0

    nch.mutate()

    mutated = nch.forgetting
    assertEquals(DontForget, mutated)

    NeuronGenome.forgetAllProbability = 0.0
    NeuronGenome.dontForgetProbability = 0.0

    nch.mutate()

    nch.forgetting match {
      case ForgetValue(value) => println("ok")
      case other => fail(s"The forgetting value should be some intermediate value, not $other")
    }
  }

  @Test def shouldMutateHush(){
    engine.hushRange = 1 to 1
    val nch = engine.tossForNeuron("id1")
    val original = nch.hushValue
    assertEquals(HushValue(1), nch.hushValue)

    NeuronGenome.thresholdProbability = 0.0
    NeuronGenome.slopeProbability = 0.0
    NeuronGenome.forgettingProbability = 0.0
    NeuronGenome.hushProbability = 1.0
    NeuronGenome.synapseChangeProbability = 0.0
    NeuronGenome.hushRange = 2 to 5

    nch.mutate()
    val mutated = nch.hushValue
    assertNotEquals(original, mutated)
    assertTrue(NeuronGenome.hushRange.contains(mutated.iterations))
  }

  @Test def shouldAddSynapse(): Unit ={
    val idSet = Set("id1","id2")
    val accessMap = Map("id1" -> MutationAccess.FULL, "id2" -> MutationAccess.FULL)
    val nch1 = engine.tossForNeuron("id1", accessMap)
    val nch2 = engine.tossForNeuron("id2", accessMap)

    assertEquals(0, nch1.synapses.size)

    NeuronGenome.thresholdProbability = 0.0
    NeuronGenome.slopeProbability = 0.0
    NeuronGenome.forgettingProbability = 0.0
    NeuronGenome.hushProbability = 0.0
    NeuronGenome.synapseChangeProbability = 1.0

    NeuronGenome.addSynapseProbability = 1.0
    NeuronGenome.removeSynapseProbability = 0.0

    nch1.mutate()

    assertEquals(1, nch1.synapses.size)
    val s = nch1.synapses(0)
    assertTrue(idSet.contains(s.neuronId))

    // a mutation should not result in adding a second synapse pointing to an already connected neuron
    // so if we mutate this neuron again, in 100% cases it should result in connections both to id1 and id2
    nch1.mutate()

    assertEquals(2, nch1.synapses.size)
    assertEquals(idSet, nch1.synapses.map(_.neuronId).toSet)

    val oldWayOfDoingCopy = NeuronGenome(nch1.data, accessMap)
    val clone = nch1.clone
    assertEquals(oldWayOfDoingCopy.data, clone.data)
    assertEquals(oldWayOfDoingCopy.accessMap, clone.accessMap)

    // nothing should change as there is no way to add another synapse
    nch1.mutate()

    assertEquals(clone.data, nch1.data)
  }

  @Test def shouldRemoveSynapse(): Unit ={
    val idSet = Set("id1","id2")
    val accessMap = Map("id1" -> MutationAccess.FULL, "id2" -> MutationAccess.FULL)
    val nch1 = engine.tossForNeuron("id1", accessMap)
    val nch2 = engine.tossForNeuron("id2", accessMap)

    nch1.addSynapse(SynapseGenome("id2",Hush))

    assertEquals(1, nch1.synapses.size)

    NeuronGenome.thresholdProbability = 0.0
    NeuronGenome.slopeProbability = 0.0
    NeuronGenome.forgettingProbability = 0.0
    NeuronGenome.hushProbability = 0.0
    NeuronGenome.synapseChangeProbability = 1.0

    NeuronGenome.addSynapseProbability = 0.0
    NeuronGenome.removeSynapseProbability = 1.0

    nch1.mutate()
    assertEquals(0, nch1.synapses.size)

    val clone = nch1.clone

    // nothing should change as there is no more synapses to remove
    nch1.mutate()
    assertEquals(0, nch1.synapses.size)
    assertEquals(clone.data, nch1.data)
  }

  @Test def shouldChangeSynapseWeight(): Unit ={
    val idSet = Set("id1","id2")
    val accessMap = Map("id1" -> MutationAccess.FULL, "id2" -> MutationAccess.FULL)
    val nch1 = engine.tossForNeuron("id1", accessMap)
    val nch2 = engine.tossForNeuron("id2", accessMap)

    nch1.addSynapse(SynapseGenome("id2",Hush))

    assertEquals(1, nch1.synapses.size)

    NeuronGenome.thresholdProbability = 0.0
    NeuronGenome.slopeProbability = 0.0
    NeuronGenome.forgettingProbability = 0.0
    NeuronGenome.hushProbability = 0.0
    NeuronGenome.synapseChangeProbability = 1.0

    NeuronGenome.addSynapseProbability = 0.0
    NeuronGenome.removeSynapseProbability = 0.0

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
    engine.neuronsRange = 2 to 2
    engine.inputIds = List("in1")
    engine.outputIds = List("out1")
    engine.synapsesDensity = 2.5
    engine.inputTickMultiplierRange = 2.0 <=> 2.0

    val netChromosome = engine.tossForNet("net")
    assertEquals(2, netChromosome.data.neurons.size)
    assertNotEquals(None, netChromosome.find("in1"))
    assertNotEquals(None, netChromosome.find("out1"))

    NetGenome.addNeuronProbability = 1.0
    NetGenome.deleteNeuronProbability = 0.0
    NetGenome.mutateNeuronProbability = 0.0
    NetGenome.inputTickMultiplierProbability = 0.0

    netChromosome.mutate()

    val middle1Opt = netChromosome.find("net1")
    assertNotEquals(None, middle1Opt)
    assertNotEquals(None, netChromosome.findSynapse("in1","net1"))
    assertNotEquals(None, netChromosome.findSynapse("net1","out1"))

/*
    netChromosome.mutate()

    assertEquals(4, netChromosome.data.neurons.size)
    val idSet = Set("in1","out1","net1")
    val newNeuronList = netChromosome.data.neurons.filterNot( n => idSet.contains(n.id) )
    assertEquals(1, newNeuronList.size)
    val newNeuron = newNeuronList(0)
*/

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