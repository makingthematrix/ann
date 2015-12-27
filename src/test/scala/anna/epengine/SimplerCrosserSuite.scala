package anna.epengine

import anna.Context
import anna.async.NetBuilder
import anna.logger.LOG
import org.junit.{After, Before, Test}
import org.scalatest.junit.JUnitSuite
import anna.async.NetBuilderOps._
import org.junit.Assert._

/**
  * Created by gorywoda on 12/27/15.
  */
class SimplerCrosserSuite extends JUnitSuite {

  private var _oldContext:Context = _

  @Before def before() {
    _oldContext = Context()
    LOG.addLogToStdout()
  }

  @After def after(): Unit ={
    Context.set(_oldContext)
  }

  private lazy val netTemplate1 = {
    val builder = NetBuilder()
    builder.netName = "net1"
    builder.addInput("in1").chain("net1_mi1",1.0,0.0).chain("net1_mi2",1.0,0.0).chain("out1",0.5,0.81)
    builder.use("in1").chain("net1_mi3",1.0,0.0).chain("net1_mi4",1.0,0.0).connect("out1",1.0)
    builder.data
  }

  private lazy val netTemplate2 = {
    val builder = NetBuilder()
    builder.netName = "net2"
    builder.addInput("in1").chain("net2_mi1",1.0,0.0).chain("net2_mi2",1.0,0.0).chain("out1",0.5,0.81)
    builder.use("in1").chain("net2_mi3",1.0,0.0).chain("net2_mi4",1.0,0.0).connect("out1",1.0)
    builder.data
  }

  private lazy val accessMap = AccessMap("in1","out1")

  @Test def shouldCopyBestAndCloneOtherGenomes(): Unit ={
    Context.withCrossCoefficient(0.0)

    val g1 = NetGenome(netTemplate1, accessMap).netId("g1")
    val g2 = NetGenome(netTemplate1, accessMap).netId("g2")
    val g3 = NetGenome(netTemplate1, accessMap).netId("g3")

    val poll = GenomePoll(List(g1, g2, g3))
    val results = Map("g1"->1.0, "g2"->0.5, "g3"->0.75)

    val crosser = Crosser(SimplerCrosser.ID, poll, results)
    val newGeneration = crosser.newGeneration(1)

    assertTrue(newGeneration.exists(_.id == "g1"))
    assertEquals(2, newGeneration.filter(_.id.endsWith("Cloned")).size)

    println(g1.toJson)
    println("---")
    println(newGeneration.find(_.id.endsWith("Cloned")).get.toJson)
  }
}
