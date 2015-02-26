package test.async.epengine

import anna.async.{NetRef, NetInput, NetBuilder}
import anna.data.NetData
import anna.epengine._
import anna.logger.LOG
import anna.logger.LOG._
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import anna.utils.Utils
import anna.async.NetBuilderOps._
import anna.epengine.MutationAccess._

class EngineSuite extends JUnitSuite {
  @Before def before() {
    LOG.addLogToStdout()
  }

  val inputIds = List("in1")
  val outputIds = List("out1")

  val anySignalAnyResponse = (ni: NetInput, ref: NetRef, good: Double, bad: Double) => {
    var counter = 0
    ref.addAfterFire("out1"){ counter += 1 }

    ni += "1"

    ni.tickUntilCalm()
    if(counter > 0) good else bad
  }

  val t1 = NetTest("any response to any signal", 1, List("out1"), anySignalAnyResponse)

  @Test def shouldTestGenomePoll(): Unit ={
    val poll = GenomePoll("net", inputIds, outputIds, 10)
    assertEquals(10, poll.genomes.size)

    val results = Tester(List(t1)).test(poll)
    results.foreach( tuple => println(s"${tuple._1.id}: ${tuple._2}"))
  }

  @Test def shouldSplitIdsRandomly(): Unit ={
    val ids = "a, b, c, d, e".split(", ").toSet
    assertEquals(Set("a","b","c","d","e"), ids)

    val (ids1, ids2) = Utils.splitIdsRandomly(ids, 2)
    assertTrue(ids1.nonEmpty)
    assertTrue(ids2.nonEmpty)
    assertEquals(ids, ids1 ++ ids2)
  }

  @Test def shouldSplitWithEmptyIds(): Unit = {
    val ids = Set[String]()
    val (ids1, ids2) = Utils.splitIdsRandomly(ids, 2)
    assertTrue(ids1.isEmpty)
    assertTrue(ids2.isEmpty)
  }

  @Test def shouldCreateNonEmptyGenomes(): Unit ={
    val poll = GenomePoll("net", inputIds, outputIds, 2)
    assertEquals(2, poll.genomes.size)
    val net1G = poll.genomes(0)
    println(s"net1G size: ${net1G.data.neurons.size}")
    val net1Middle = net1G.filterNot(inputIds ++ outputIds)
    assertTrue(net1Middle.nonEmpty)
    val net2G = poll.genomes(1)
    println(s"net2G size: ${net2G.data.neurons.size}")
    val net2Middle = net2G.filterNot(inputIds ++ outputIds)
    assertTrue(net2Middle.nonEmpty)
  }

  @Test def shouldAssignFullAccessToMiddleNeurons(): Unit ={
    val poll = GenomePoll("net", inputIds, outputIds, 1)
    val netG = poll.genomes(0)
    val net1Middle = netG.filterNot(inputIds ++ outputIds)
    val net1FullAccess = netG.fullAccessNeurons()
    assertEquals(net1Middle, net1FullAccess)
  }

  @Test def shouldCreateNewGenome(): Unit ={
    val poll = GenomePoll("net", inputIds, outputIds, 2)
    val net1G = poll.genomes(0)
    val net2G = poll.genomes(1)

    val variables = net1G.fullAccessNeurons() ++ net2G.fullAccessNeurons()

    val ids = variables.map(_.id).toSet
    println(s"ids: ${ids.mkString(", ")}")
    val (ids1, ids2) = Utils.splitIdsRandomly(ids, 2)
    println(s"ids1: ${ids1.mkString(", ")}")
    val newGen = NetGenome.breed(net1G, variables, ids1, false)

    val expectedIds = (ids1.map(id => NetData.replaceNetId(id, net1G.id))) ++ net1G.notFullAccessNeurons().map(_.id).toSet
    println(s"expectedIds: ${expectedIds.mkString(", ")}")
    val newGenIds = newGen.neurons.map(_.id).toSet
    println(s"newGenIds: ${newGenIds.mkString(", ")}")

    assertEquals(expectedIds.size, newGenIds.size)
    assertFalse(expectedIds.exists(!newGenIds.contains(_)))
    assertFalse(newGenIds.exists(!expectedIds.contains(_)))
  }

  @Test def shouldCrossTwoEvenGenomes(): Unit ={
    val b1 = NetBuilder()
    b1.netName = "net1"
    b1.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    b1.use("in1").chain("net1_3",1.0,0.0).chain("net1_4",1.0,0.0).connect("out1",1.0)
    val net1G = NetGenome(b1.data, Map("in1" -> DONTMUTATE, "out1" -> DONTDELETE))

    val b2 = NetBuilder()
    b2.netName = "net2"
    b2.addInput("in1").chain("net2_1",1.0,0.0).chain("net2_2",1.0,0.0).chain("out1",0.5,0.81)
    b2.use("in1").chain("net2_3",1.0,0.0).chain("net2_4",1.0,0.0).connect("out1",1.0)
    val net2G = NetGenome(b2.data, Map("in1" -> DONTMUTATE, "out1" -> DONTDELETE))

    crossTest(net1G, net2G)
  }

  @Test def shouldCrossTwoUnevenGenomes(): Unit ={
    val b1 = NetBuilder()
    b1.netName = "net1"
    b1.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    val net1G = NetGenome(b1.data, Map("in1" -> DONTMUTATE, "out1" -> DONTDELETE))

    val b2 = NetBuilder()
    b2.netName = "net2"
    b2.addInput("in1").chain("net2_1",1.0,0.0).chain("net2_2",1.0,0.0).chain("out1",0.5,0.81)
    b2.use("in1").chain("net2_3",1.0,0.0).chain("net2_4",1.0,0.0).connect("out1",1.0)
    val net2G = NetGenome(b2.data, Map("in1" -> DONTMUTATE, "out1" -> DONTDELETE))

    crossTest(net1G, net2G)
  }

  @Test def shouldNotCrossGenomesWithoutCommonNeurons(): Unit = {
    val b1 = NetBuilder()
    b1.netName = "net1"
    b1.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    b1.use("in1").chain("net1_3",1.0,0.0).chain("net1_4",1.0,0.0).connect("out1",1.0)
    val net1G = NetGenome(b1.data, Map("in1" -> DONTMUTATE, "out1" -> DONTDELETE))

    val b2 = NetBuilder()
    b2.netName = "net2"
    b2.addInput("in1").chain("net2_11",1.0,0.0).chain("net2_12",1.0,0.0).chain("out1",0.5,0.81)
    b2.use("in1").chain("net2_13",1.0,0.0).chain("net2_14",1.0,0.0).connect("out1",1.0)
    val net2G = NetGenome(b2.data, Map("in1" -> DONTMUTATE, "out1" -> DONTDELETE))

    assertFalse(net1G.crossable(net2G))

    val (net12G, net21G) = net1G.cross(net2G, false, false)
    assertEquals(net1G.data, net12G.data)
    assertEquals(net2G.data, net21G.data)
  }

  @Test def shouldNotCrossGenomesWithDifferentIO(): Unit = {
    val b1 = NetBuilder()
    b1.netName = "net1"
    b1.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    b1.use("in1").chain("net1_3",1.0,0.0).chain("net1_4",1.0,0.0).connect("out1",1.0)
    val net1G = NetGenome(b1.data, Map("in1" -> DONTMUTATE, "out1" -> DONTDELETE))

    val b2 = NetBuilder()
    b2.netName = "net2"
    b2.addInput("in2").chain("net2_1",1.0,0.0).chain("net2_2",1.0,0.0).chain("out2",0.5,0.81)
    b2.use("in2").chain("net2_3",1.0,0.0).chain("net2_4",1.0,0.0).connect("out2",1.0)
    val net2G = NetGenome(b2.data, Map("in2" -> DONTMUTATE, "out2" -> DONTDELETE))

    assertFalse(net1G.crossable(net2G))

    val (net12G, net21G) = net1G.cross(net2G, false, false)
    assertEquals(net1G.data, net12G.data)
    assertEquals(net2G.data, net21G.data)
  }

  private def crossTest(net1G: NetGenome, net2G: NetGenome): Unit ={
    val net1Middle = net1G.filterNot(inputIds ++ outputIds).map(_.id)
    assertTrue(net1Middle.nonEmpty)
    val net2Middle = net2G.filterNot(inputIds ++ outputIds).map(_.id)
    assertTrue(net2Middle.nonEmpty)

    debug("---")
    debug("net1G: " + net1G.neurons.map(_.id))
    debug("net2G: " + net2G.neurons.map(_.id))
    val (net12G, net21G) = net1G.cross(net2G, false, false)
    debug("net12G: " + net12G.neurons.map(_.id))
    debug("net21G: " + net21G.neurons.map(_.id))
    debug("---")
    // has the same input neurons
    assertEquals(net12G.inputs, inputIds)
    // has the same output neurons
    assertFalse(outputIds.exists( net12G.find(_) == None ))
    // has non-empty middle layer
    val net12Middle = net12G.filterNot(inputIds ++ outputIds).map(_.id)
    assertTrue(net12Middle.nonEmpty)

    // has the same input neurons
    assertEquals(net21G.inputs, inputIds)
    // has the same output neurons
    assertFalse(outputIds.exists( net21G.find(_) == None ))
    // has non-empty middle layer
    val net21Middle = net21G.filterNot(inputIds ++ outputIds).map(_.id)
    assertTrue(net21Middle.nonEmpty)

    // there should be no shared neurons
    assertEquals(Nil, net12Middle.intersect(net21Middle))

    val net1xnet12 = net1Middle.intersect(net12Middle)
    val net2xnet12 = net2Middle.intersect(net12Middle)
    assertEquals(Nil, net1xnet12.intersect(net2xnet12))

    val net1xnet21 = net1Middle.intersect(net21Middle)
    val net2xnet21 = net2Middle.intersect(net21Middle)
    assertEquals(Nil, net1xnet21.intersect(net2xnet21))

    debug(this,s"net1Middle: $net1Middle")
    debug(this,s"net12Middle: $net12Middle")
    debug(this,s"net21Middle: $net21Middle")
    debug(this,s"net1xnet12: $net1xnet12")
    debug(this,s"net1xnet21: $net1xnet21")
    assertEquals(net1Middle.toSet, (net1xnet12 ++ net1xnet21).toSet)
    assertEquals(net2Middle.toSet, (net2xnet12 ++ net2xnet21).toSet)
  }
}