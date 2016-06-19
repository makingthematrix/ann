package anna.epengine

import anna.Context
import anna.async.{MySuite, NetBuilder}
import anna.async.NetBuilderOps._
import anna.data.{Hush, SynapseWeight}
import anna.logger.LOG
import anna.logger.LOG._
import anna.utils.DoubleRange._
import org.junit.Assert._
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite

/**
 * Created by gorywoda on 16.02.15.
 */
class SynapseGenomeSuite extends MySuite {
  @Before override def before() {
    LOG.addLogToStdout()

    Context.withWeightRange(-1.0 <=> 1.0)
    Context.withHushProbability(0.1)
  }

  @Test def shouldTossForSynapse() {
    debug(this, "shouldTossForSynapse")
    val totalCount = 1000
    var hushCount = 0
    for (i <- 1 to totalCount) {
      val sg = SynapseGenome.build("id1")
      assertEquals("id1", sg.neuronId)
      sg.weight match {
        case Hush() => hushCount = hushCount + 1
        case SynapseWeight(w) =>
          assert(Context().weightRange.contains(w), s"weight outside range: $w")
      }
    }

    assertTrue(hushCount > 70)
    assertTrue(hushCount < 130)
  }

  @Test def shouldChangeHushProbability() {
    debug(this, "shouldChangeHushProbability")
    Context.withHushProbability(1.0)
    Context.withFullWeightProbability(0.0)
    Context.withInvertSynapseProbability(0.0)

    val totalCount = 1000
    var hushCount = 0
    for (i <- 1 to totalCount) {
      val sg = SynapseGenome.build("id1")
      assertEquals("id1", sg.neuronId)
      sg.weight match {
        case Hush() => hushCount += 1
        case SynapseWeight(w) =>
          assert(Context().weightRange.contains(w), s"weight outside range: $w")
      }
    }

    assertTrue(hushCount > 800)
  }

  @Test def shouldMutateSynapse(): Unit ={
    debug(this,"shouldMutateSynapse")
    val data = NetBuilder().addInput("in").chain("out",0.5,0.81).data
    val gen = NetGenome(data, Map("in" -> MutationAccessInput(), "out" -> MutationAccessOutput()))
    val sg = gen.neurons.find(_.id == "in").get.synapses.find(_.neuronId == "out").get

    val originalWeight = sg.weight

    MutationsProfile(
      "invertSynapse" -> 0.1,
      "setWeightToHush" -> 0.2,
      "setWeightToFull" -> 0.2,
      "mutateWeight" -> 0.5
    ).mutate(gen)

    assertNotEquals(originalWeight, sg.weight)
  }

}
