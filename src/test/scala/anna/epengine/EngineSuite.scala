package test.async.epengine

import anna.async.{NetRef, NetInput, NetBuilder}
import anna.data.NetData
import anna.epengine._
import anna.logger.LOG
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import anna.utils.Utils

class EngineSuite extends JUnitSuite {
  @Before def before() {
    LOG.addLogToStdout()

    LOG.track(Tester.getClass())
    LOG.trackAll = false
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
    val newGen = NetGenome.createNewGenome(net1G, variables, ids1, List(net1G.id,net2G.id), false)

    val expectedIds = (ids1.map(id => NetData.replaceNetId(id, net1G.id))) ++ net1G.notFullAccessNeurons().map(_.id).toSet
    println(s"expectedIds: ${expectedIds.mkString(", ")}")
    val newGenIds = newGen.neurons.map(_.id).toSet
    println(s"newGenIds: ${newGenIds.mkString(", ")}")

    assertEquals(expectedIds.size, newGenIds.size)
    assertFalse(expectedIds.exists(!newGenIds.contains(_)))
    assertFalse(newGenIds.exists(!expectedIds.contains(_)))
  }

  @Test def shouldCrossTwoGenomes(): Unit ={
    val poll = GenomePoll("net", inputIds, outputIds, 2)
    val net1G = poll.genomes(0)
    println(s"net1G size: ${net1G.data.neurons.size}")
    val net1Middle = net1G.filterNot(inputIds ++ outputIds)
    assertTrue(net1Middle.nonEmpty)
    val net2G = poll.genomes(1)
    println(s"net2G size: ${net2G.data.neurons.size}")
    val net2Middle = net2G.filterNot(inputIds ++ outputIds)
    assertTrue(net2Middle.nonEmpty)

    val (net12G, net21G) = NetGenome.cross(net1G, net2G)

    // has the same input neurons
    assertEquals(net12G.inputs, inputIds)
    // has the same output neurons
    assertFalse(outputIds.exists( net12G.find(_) == None ))
    // has non-empty middle layer
    val net12Middle = net12G.filterNot(inputIds ++ outputIds)
    assertTrue(net12Middle.nonEmpty)

    // has the same input neurons
    assertEquals(net21G.inputs, inputIds)
    // has the same output neurons
    assertFalse(outputIds.exists( net21G.find(_) == None ))
    // has non-empty middle layer
    val net21Middle = net21G.filterNot(inputIds ++ outputIds)
    assertTrue(net21Middle.nonEmpty)

    // there should be no shared neurons
    assertEquals(Nil, net12Middle.map(_.id).intersect(net21Middle.map(_.id)))

    val net1xnet12 = net1Middle.map(_.id).intersect(net12Middle.map(_.id))
    val net2xnet12 = net2Middle.map(_.id).intersect(net12Middle.map(_.id))
    assertEquals(Nil, net1xnet12.intersect(net2xnet12))

    val net1xnet21 = net1Middle.map(_.id).intersect(net21Middle.map(_.id))
    val net2xnet21 = net2Middle.map(_.id).intersect(net21Middle.map(_.id))
    assertEquals(Nil, net1xnet21.intersect(net2xnet21))

    assertEquals(net1Middle.toSet, (net1xnet12 ++ net1xnet21).toSet)
    assertEquals(net2Middle.toSet, (net2xnet12 ++ net2xnet21).toSet)
  }
}