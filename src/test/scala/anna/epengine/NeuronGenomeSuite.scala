package anna.epengine

import anna.Context
import anna.data._
import anna.logger.LOG
import anna.utils.DoubleRange._
import org.junit.Assert._
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite

/**
 * Created by gorywoda on 16.02.15.
 */
class NeuronGenomeSuite extends JUnitSuite {

  @Before def before() {
    LOG.addLogToStdout()

    Context.withThresholdRange(0.0 <=> 0.9)
    Context.withSlopeRange(1.0 <=> 20.0)
    Context.withHushRange(1 to 5)
    Context.withForgettingRange(0.1 <=> 0.9)
    Context.withDontForgetProbability(0.75)
    Context.withForgetAllProbability(0.05)
    Context.withTickTimeMultiplierRange(0.5 <=> 2.0)
  }

  @Test def shouldTossForNeuron(): Unit = {
    Context.withThresholdRange(0.0 <=> 0.9)
    Context.withSlopeRange(1.0 <=> 20.0)
    Context.withHushRange(1 to 5)
    Context.withForgettingRange(0.1 <=> 0.9)
    Context.withDontForgetProbability(0.75)
    Context.withForgetAllProbability(0.05)
    Context.withTickTimeMultiplierRange(0.5 <=> 2.0)

    val totalCount = 1000
    var dontForgetCount = 0
    var forgetAllCount = 0
    for (i <- 1 to totalCount) {
      val ng = NeuronGenome.build("id1")
      assertEquals("id1", ng.id)
      assertTrue(Context().thresholdRange.contains(ng.threshold))
      assertTrue(Context().slopeRange.contains(ng.slope))
      assertTrue(Context().hushRange.contains(ng.hushValue.iterations))
      ng.forgetting match {
        case DontForget() => dontForgetCount += 1
        case ForgetAll() => forgetAllCount += 1
        case ForgetValue(value) => assertTrue(Context().forgettingRange.contains(value))
      }
    }
    assertTrue((600 to 800).contains(dontForgetCount))
    assertTrue((0 to 100).contains(forgetAllCount))
  }

  @Test def shouldMutateThreshold(): Unit ={
    val ng = NeuronGenome.build("id1")
    val original = ng.threshold

    Context.withThresholdProbability(1.0)
    Context.withSlopeProbability(0.0)
    Context.withForgettingProbability(0.0)
    Context.withHushValueProbability(0.0)
    Context.withSynapseChangeProbability(0.0)
    Context.withTickTimeMultiplierProbability(0.0)

    ng.mutate()

    val mutated = ng.threshold
    assertNotEquals(original, mutated)
  }

  @Test def shouldMutateSlope(): Unit ={
    val ng = NeuronGenome.build("id1")
    val original = ng.slope

    Context.withThresholdProbability(0.0)
    Context.withSlopeProbability(1.0)
    Context.withForgettingProbability(0.0)
    Context.withHushValueProbability(0.0)
    Context.withSynapseChangeProbability(0.0)
    Context.withTickTimeMultiplierProbability(0.0)

    ng.mutate()

    val mutated = ng.slope
    assertNotEquals(original, mutated)
  }


  @Test def shouldMutateForgetting(): Unit ={
    val ng = NeuronGenome.build("id1")
    val original = ng.forgetting

    Context.withThresholdProbability(0.0)
    Context.withSlopeProbability(0.0)
    Context.withForgettingProbability(1.0)
    Context.withHushValueProbability(0.0)
    Context.withSynapseChangeProbability(0.0)
    Context.withTickTimeMultiplierProbability(0.0)

    Context.withForgetAllProbability(1.0)
    Context.withDontForgetProbability(0.0)

    ng.mutate()

    var mutated = ng.forgetting
    assertEquals(ForgetAll(), mutated)

    Context.withForgetAllProbability(0.0)
    Context.withDontForgetProbability(1.0)

    ng.mutate()

    mutated = ng.forgetting
    assertEquals(DontForget(), mutated)

    Context.withForgetAllProbability(0.0)
    Context.withDontForgetProbability(0.0)

    ng.mutate()

    ng.forgetting match {
      case ForgetValue(value) => println("ok")
      case other => fail(s"The forgetting value should be some intermediate value, not $other")
    }
  }

  @Test def shouldMutateHush(){
    Context.withHushRange(1 to 1)
    val ng = NeuronGenome.build("id1")
    val original = ng.hushValue
    assertEquals(HushValue(1), ng.hushValue)

    Context.withThresholdProbability(0.0)
    Context.withSlopeProbability(0.0)
    Context.withForgettingProbability(0.0)
    Context.withHushValueProbability(1.0)
    Context.withSynapseChangeProbability(0.0)
    Context.withTickTimeMultiplierProbability(0.0)

    Context.withHushRange(2 to 5)

    ng.mutate()
    val mutated = ng.hushValue
    assertNotEquals(original, mutated)
    assertTrue(Context().hushRange.contains(mutated.iterations))
  }

  @Test def shouldAddSynapse(): Unit ={
    val idSet = Set("id1","id2")
    val accessMap = Map("id1" -> MutationAccessFull(), "id2" -> MutationAccessFull())
    val ng1 = NeuronGenome.build("id1", accessMap)
    val ng2 = NeuronGenome.build("id2", accessMap)

    assertEquals(0, ng1.synapses.size)

    Context.withThresholdProbability(0.0)
    Context.withSlopeProbability(0.0)
    Context.withForgettingProbability(0.0)
    Context.withHushValueProbability(0.0)
    Context.withSynapseChangeProbability(1.0)
    Context.withTickTimeMultiplierProbability(0.0)

    Context.withAddSynapseProbability(1.0)
    Context.withDeleteSynapseProbability(0.0)

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
    val accessMap = Map("id1" -> MutationAccessFull(), "id2" -> MutationAccessFull())
    val ng1 = NeuronGenome.build("id1", accessMap)
    val ng2 = NeuronGenome.build("id2", accessMap)

    ng1.addSynapse(SynapseGenome("id2",Hush()))

    assertEquals(1, ng1.synapses.size)

    Context.withThresholdProbability(0.0)
    Context.withSlopeProbability(0.0)
    Context.withForgettingProbability(0.0)
    Context.withHushValueProbability(0.0)
    Context.withSynapseChangeProbability(1.0)
    Context.withTickTimeMultiplierProbability(0.0)

    Context.withAddSynapseProbability(0.0)
    Context.withDeleteSynapseProbability(1.0)

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
    val accessMap = Map("id1" -> MutationAccessFull(), "id2" -> MutationAccessFull())
    val nch1 = NeuronGenome.build("id1", accessMap)
    val nch2 = NeuronGenome.build("id2", accessMap)

    nch1.addSynapse(SynapseGenome("id2",Hush()))

    assertEquals(1, nch1.synapses.size)

    Context.withThresholdProbability(0.0)
    Context.withSlopeProbability(0.0)
    Context.withForgettingProbability(0.0)
    Context.withHushValueProbability(0.0)
    Context.withSynapseChangeProbability(1.0)
    Context.withTickTimeMultiplierProbability(0.0)

    Context.withAddSynapseProbability(0.0)
    Context.withDeleteSynapseProbability(0.0)

    Context.withFullWeightProbability(1.0)
    Context.withHushProbability(0.0)

    nch1.mutate()
    assertEquals(SynapseWeight(1.0), nch1.getSynapse("id2").weight)

    Context.withFullWeightProbability(0.0)
    Context.withHushProbability(1.0)

    nch1.mutate()
    assertEquals(Hush(), nch1.getSynapse("id2").weight)

    Context.withFullWeightProbability(0.0)
    Context.withHushProbability(0.0)

    nch1.mutate()
    val weight = nch1.getSynapse("id2").weight
    assertNotEquals(Hush, weight)
    assertNotEquals(SynapseWeight(1.0), weight)
  }
}
