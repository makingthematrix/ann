package anna.epengine

import anna.Context
import anna.async.NetBuilder
import anna.data.{Hush, SynapseWeight}
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
    builder.netId = "net1"
    builder.addInput("in1").chain("net1_mi1",1.0,0.0).chain("net1_mi2",1.0,0.0).chain("out1",0.5,0.81)
    builder.use("in1").chain("net1_mi3",1.0,0.0).chain("net1_mi4",1.0,0.0).connect("out1",1.0)
    builder.data
  }

  private lazy val netTemplate2 = {
    val builder = NetBuilder()
    builder.netId = "net2"
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
    val newGeneration = crosser.newGeneration()

    assertTrue(newGeneration.exists(_.id == "g1"))
    assertEquals(2, newGeneration.filter(_.id.endsWith("Cloned")).size)
  }

  @Test def shouldSwitchCommonNeuronsThreshold(): Unit = {
    Context.withCrossCoefficient(0.0)

    val DATA1_THRESHOLD = 0.0
    val DATA2_THRESHOLD = 1.0

    val data1 = NetBuilder().addInput("in1").chain("mi1",1.0,DATA1_THRESHOLD).chain("out1",0.5,0.81).netId("data1").data
    val ng1 = NetGenome(data1, accessMap)
    val data2 = NetBuilder().addInput("in1").chain("mi1",1.0,DATA2_THRESHOLD).chain("out1",0.5,0.81).netId("data2").data
    val ng2 = NetGenome(data2, accessMap)

    val crosser = Crosser(SimplerCrosser.ID, GenomePoll(Nil), Map())
    val (new1, new2) = crosser.cross(ng1, ng2)

    assertEquals(DATA2_THRESHOLD, new1.data.neuron("mi1").threshold, 0.01)
    assertEquals(DATA1_THRESHOLD, new2.data.neuron("mi1").threshold, 0.01)
  }

  @Test def shouldSwitchCommonNeuronsSynapse(): Unit = {
    Context.withCrossCoefficient(0.0)

    val DATA1_WEIGHT = Hush()
    val DATA2_WEIGHT = SynapseWeight(1.0)

    val data1 = NetBuilder().addInput("in1").chain("mi1",1.0, 0.5).chainHush("out1",0.81).netId("data1").data
    val ng1 = NetGenome(data1, accessMap)
    val data2 = NetBuilder().addInput("in1").chain("mi1", 1.0, 0.5).chain("out1",1.0,0.81).netId("data2").data
    val ng2 = NetGenome(data2, accessMap)

    assertEquals(DATA1_WEIGHT, ng1.findSynapse("mi1","out1").weight)
    assertEquals(DATA2_WEIGHT, ng2.findSynapse("mi1","out1").weight)

    val crosser = Crosser(SimplerCrosser.ID, GenomePoll(Nil), Map())
    val (new1, new2) = crosser.cross(ng1, ng2)

    assertEquals(DATA2_WEIGHT, ng1.findSynapse("mi1","out1").weight)
    assertEquals(DATA1_WEIGHT, ng2.findSynapse("mi1","out1").weight)
  }

  @Test def shouldCrossOnlyTwoBestOnes(): Unit = {
    Context.withCrossCoefficient(1.0)

    case class Data(id: String, t: Double, result: Double)
    val T1 = Data("ng1", 0.1, 1.0)
    val T2 = Data("ng2", 0.2, 0.9)
    val T3 = Data("ng3", 0.3, 0.0)
    val T4 = Data("ng4", 0.4, 0.0)

    val ng1 = NetGenome(NetBuilder().addInput("in1").chain("mi1",1.0,T1.t).chain("out1",0.5,0.81).netId(T1.id).data, accessMap)
    val ng2 = NetGenome(NetBuilder().addInput("in1").chain("mi1",1.0,T2.t).chain("out1",0.5,0.81).netId(T2.id).data, accessMap)
    val ng3 = NetGenome(NetBuilder().addInput("in1").chain("mi1",1.0,T3.t).chain("out1",0.5,0.81).netId(T3.id).data, accessMap)
    val ng4 = NetGenome(NetBuilder().addInput("in1").chain("mi1",1.0,T4.t).chain("out1",0.5,0.81).netId(T4.id).data, accessMap)

    val results = Map(T1.id -> T1.result, T2.id -> T2.result, T3.id -> T3.result, T4.id -> T4.result)

    val crosser = Crosser(SimplerCrosser.ID, GenomePoll(ng1, ng2, ng3, ng4), results)

    val newGeneration = crosser.newGeneration()
    assertEquals(4, newGeneration.size)

    assertEquals(2, crosser.crossed.size)
    assertEquals(0, crosser.cloned.size)

    assertTrue(crosser.crossed.contains(T1.id))
    assertTrue(crosser.crossed.contains(T2.id))

    assertTrue(newGeneration.exists(_.id == T1.id)) // the best is copied, not cloned
    LOG.debug(this, s"newGeneration: ${newGeneration.map(_.id)}")
  }
}
