package anna.epengine

import anna.data._
import anna.logger.LOG
import org.junit.Assert._
import org.junit.{Test, Before}
import org.scalatest.junit.JUnitSuite
import anna.utils.DoubleRange._
import anna.Context

/**
 * Created by gorywoda on 16.02.15.
 */
class NeuronGenomeSuite extends JUnitSuite {

  @Before def before() {
    LOG.addLogToStdout()

    NeuronGenome.thresholdRange = 0.0 <=> 0.9
    NeuronGenome.slopeRange = 1.0 <=> 20.0
    NeuronGenome.hushRange = 1 to 5
    NeuronGenome.forgettingRange = 0.1 <=> 0.9
    NeuronGenome.dontForgetProbability = 0.75
    NeuronGenome.forgetAllProbability = 0.05
    NeuronGenome.tickTimeMultiplierRange = 0.5 <=> 2.0
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

    Context.withFullWeightProbability(1.0)
    Context.withHushProbability(0.0)

    nch1.mutate()
    assertEquals(SynapseWeight(1.0), nch1.getSynapse("id2").weight)

    Context.withFullWeightProbability(0.0)
    Context.withHushProbability(1.0)

    nch1.mutate()
    assertEquals(Hush, nch1.getSynapse("id2").weight)

    Context.withFullWeightProbability(0.0)
    Context.withHushProbability(0.0)

    nch1.mutate()
    val weight = nch1.getSynapse("id2").weight
    assertNotEquals(Hush, weight)
    assertNotEquals(SynapseWeight(1.0), weight)
  }
}
