package anna.epengine

import anna.Context
import anna.async.{MySuite, NetBuilder}
import anna.data._
import anna.logger.LOG
import anna.utils.DoubleRange._
import org.junit.Assert._
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite

/**
 * Created by gorywoda on 16.02.15.
 */
class NeuronGenomeSuite extends MySuite {

  @Before override def before() {
    LOG.addLogToStdout()
    Context.reset()
  }

  @Test def shouldTossForNeuron(): Unit = {
    Context.withThresholdRange(0.0 <=> 0.9)
    Context.withHushRange(1 to 5)
    Context.withForgettingRange(0.1 <=> 0.9)
    Context.withDontForgetProbability(0.75)
    Context.withForgetAllProbability(0.05)

    val totalCount = 1000
    var dontForgetCount = 0
    var forgetAllCount = 0
    for (i <- 1 to totalCount) {
      val ng = NeuronGenome.build("id1")
      assertEquals("id1", ng.id)
      assertTrue(Context().thresholdRange.contains(ng.threshold))
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

  @Test def shouldMutateThreshold(): Unit = {
    Context.withThresholdRange(0.0 <=> 0.9)
    Context.withHushRange(1 to 5)
    Context.withForgettingRange(0.1 <=> 0.9)
    Context.withDontForgetProbability(0.75)
    Context.withForgetAllProbability(0.05)

    val gen = NetGenome(
      NetBuilder().addMiddle("id1").data
    )

    val ng = gen.find("id1").get
    val original = ng.threshold

    MutationsProfile(
      "mutateThreshold" -> 1.0
    ).mutate(gen)

    val mutated = ng.threshold
    assertNotEquals(original, mutated)
  }

  @Test def shouldMutateForgetting(): Unit = {
    Context.withThresholdRange(0.0 <=> 0.9)
    Context.withHushRange(1 to 5)
    Context.withForgettingRange(0.1 <=> 0.9)
    Context.withDontForgetProbability(0.75)
    Context.withForgetAllProbability(0.05)

    val gen = NetGenome(
      NetBuilder().addMiddle("id1").data
    )

    val ng = gen.find("id1").get
    val original = ng.forgetting

    MutationsProfile(
      "setForgetAll" -> 1.0
    ).mutate(gen)

    var mutated = ng.forgetting
    assertEquals(ForgetAll(), mutated)

    MutationsProfile(
      "setDontForget" -> 1.0
    ).mutate(gen)

    mutated = ng.forgetting
    assertEquals(DontForget(), mutated)

    MutationsProfile(
      "mutateForgetValue" -> 1.0
    ).mutate(gen)

    ng.forgetting match {
      case ForgetValue(value) => println("ok")
      case other => fail(s"The forgetting value should be some intermediate value, not $other")
    }
  }

  @Test def shouldMutateHush(){
    Context.withThresholdRange(0.0 <=> 0.9)
    Context.withForgettingRange(0.1 <=> 0.9)
    Context.withDontForgetProbability(0.75)
    Context.withForgetAllProbability(0.05)

    Context.withHushRange(1 to 1)

    val gen = NetGenome(
      NetBuilder().addMiddle("id1").data
    )

    val ng = gen.find("id1").get

    val original = ng.hushValue
    assertEquals(HushValue(1), ng.hushValue)

    Context.withHushRange(2 to 5)

    MutationsProfile(
    "mutateHushValue" -> 1.0
    ).mutate(gen)

    val mutated = ng.hushValue
    assertNotEquals(original, mutated)
    assertTrue(Context().hushRange.contains(mutated.iterations))
  }

  @Test def shouldAddSynapse(): Unit ={
    Context.withThresholdRange(0.0 <=> 0.9)
    Context.withHushRange(1 to 5)
    Context.withForgettingRange(0.1 <=> 0.9)
    Context.withDontForgetProbability(0.75)
    Context.withForgetAllProbability(0.05)

    val gen = NetGenome(
      NetBuilder().addInput("id1").addMiddle("id2").data, Map("id1" -> MutationAccessInput())
    )

    val ng1 = gen.find("id1").get

    assertEquals(0, ng1.synapses.size)

    val mp = MutationsProfile(
      "addSynapse" -> 1.0
    )

    mp.mutate(gen)

    assertEquals(1, gen.synapses.size) // it should not be possible to connect id2->id1, because id1 is an input, but it is possible to connect id2->id2
    assertTrue(gen.synapses(0).neuronId == "id2") // id2 is the only connectable neuron

    mp.mutate(gen)
    assertEquals(2, gen.synapses.size)
    assertEquals(1, ng1.synapses.size) // two synapses is the max for this net and one if these synapses has to be id1->id2


    // a mutation should not result in adding a second synapse pointing to an already connected neuron
    // so if we mutate this neuron again, in 100% cases it should result in connections both to id1 and id2
    mp.mutate(gen)


    assertEquals(2, gen.synapses.size)
  }


  @Test def shoulDeleteSynapse(): Unit ={
    Context.withThresholdRange(0.0 <=> 0.9)
    Context.withHushRange(1 to 5)
    Context.withForgettingRange(0.1 <=> 0.9)
    Context.withDontForgetProbability(0.75)
    Context.withForgetAllProbability(0.05)

    val gen = NetGenome(
      NetBuilder().addMiddle("id1").addMiddle("id2").use("id1").hush("id2").data
    )

    val ng1 = gen.find("id1").get

    assertEquals(1, ng1.synapses.size)

    val mp = MutationsProfile(
      "deleteSynapse" -> 1.0
    )

    mp.mutate(gen)

    assertEquals(0, ng1.synapses.size)

    val clone = ng1.clone

    // nothing should change as there is no more synapses to remove
    mp.mutate(gen)
    assertEquals(0, ng1.synapses.size)
    assertEquals(clone.data, ng1.data)
  }

  @Test def shouldChangeSynapseWeight(): Unit ={
    Context.withThresholdRange(0.0 <=> 0.9)
    Context.withHushRange(1 to 5)
    Context.withForgettingRange(0.1 <=> 0.9)
    Context.withDontForgetProbability(0.75)
    Context.withForgetAllProbability(0.05)

    val gen = NetGenome(
      NetBuilder().addMiddle("id1").addMiddle("id2").use("id1").hush("id2").data
    )

    val synGen = gen.findSynapse("id1","id2")

    MutationsProfile(
      "setWeightToFull" -> 1.0
    ).mutate(gen)

    assertEquals(SynapseWeight(1.0), synGen.weight)

    MutationsProfile(
      "setWeightToHush" -> 1.0
    ).mutate(gen)

    assertEquals(Hush(), synGen.weight)

    MutationsProfile(
      "mutateWeight" -> 1.0
    ).mutate(gen)

    assertNotEquals(Hush, synGen.weight)
    assertNotEquals(SynapseWeight(1.0), synGen.weight)
  }
}
