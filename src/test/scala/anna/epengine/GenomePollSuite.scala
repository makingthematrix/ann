package anna.epengine

import anna.async.NetBuilder
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import anna.async.NetBuilderOps._
import org.junit.Assert._

/**
 * Created by gorywoda on 12/15/15.
 */
class GenomePollSuite extends JUnitSuite {
  @Test def shouldMutateWhenCreatingPoll():Unit = {
    val data = NetBuilder().addInput("in").chain("out",1.0,0.5).setName("simplest").data
    val accessMap = AccessMap(List("in"),List("out"))
    val mutationsProfile = MutationsProfile(
      "mutateWeight" -> 1.0
    )

    val orgSynapse = data.synapse("in", "out").get

    val poll = GenomePoll(data, List("in"), List("out"), 2, mutationsProfile, 1)

    val g0Synapse = poll.genomes(0).data.synapse("in", "out").get
    val g1Synapse = poll.genomes(1).data.synapse("in", "out").get
    assertNotEquals(orgSynapse.weight, g0Synapse.weight)
    assertNotEquals(orgSynapse.weight, g1Synapse.weight)
    assertNotEquals(g0Synapse.weight, g1Synapse.weight)
  }
}
