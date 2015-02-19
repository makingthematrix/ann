package anna.epengine

import anna.data.{SynapseWeight, Hush}
import anna.logger.LOG
import anna.logger.LOG._
import org.junit.Assert._
import org.junit.{Test, After, Before}
import org.scalatest.junit.JUnitSuite
import anna.utils.DoubleRange._

/**
 * Created by gorywoda on 16.02.15.
 */
class SynapseGenomeSuite extends JUnitSuite {
  @Before def before() {
    LOG.addLogToStdout()

    SynapseGenome.weightRange = -1.0 <=> 1.0
    SynapseGenome.hushProbability = 0.1
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

  @Test def shouldMutateSynapse(): Unit ={
    val sg = SynapseGenome.toss("id1")
    val originalWeight = sg.weight
    sg.mutate()
    val mutatedWeight = sg.weight
    assertNotEquals(originalWeight, mutatedWeight)
  }

}
