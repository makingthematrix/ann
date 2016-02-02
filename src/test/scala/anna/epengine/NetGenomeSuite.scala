package anna.epengine

import anna.Context
import anna.async.NetBuilder
import anna.async.NetBuilderOps._
import anna.data.{Hush, NetData, SynapseData}
import anna.logger.LOG
import anna.logger.LOG._
import anna.utils.DoubleRange._
import anna.utils.Utils
import org.junit.Assert._
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite

/**
 * Created by gorywoda on 16.02.15.
 */
class NetGenomeSuite extends JUnitSuite {
  @Before def before() {
    LOG.addLogToStdout()
  }

  val netData = NetBuilder().addInput("in1").chain("mi",1.0,1.0).chain("out1",1.0,1.0).netId("net").data
  val inputIds = List("in1")
  val outputIds = List("out1")

  @Test def shouldAddNeuron(): Unit ={
    val netDataInOut = NetBuilder().addInput("in1").chain("out1",1.0,1.0).netId("net").data
    Context.withNeuronsRange(3 to 3)
    Context.withSynapsesDensity(2.5)
    Context.withInputTickMultiplierRange(2.0 <=> 2.0)

    val ng = NetGenome(netDataInOut, AccessMap(inputIds, outputIds))
    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in1"))
    assertNotEquals(None, ng.find("out1"))

    MutationsProfile(
      "addNeuron" -> 1.0
    ).mutate(ng)

    assertEquals(3, ng.data.neurons.size)

    val middle1Opt = ng.find("net_1")
    assertNotEquals(None, middle1Opt)

    ng.find("in1").get.isConnectedTo("net_1")
    assertTrue(ng.find("in1").get.isConnectedTo("net_1") || ng.find("out1").get.isConnectedTo("net_1"))
    assertNotEquals(None, ng.findSynapse("net_1","out1"))
  }

  @Test def shouldDeleteNeuron(): Unit ={
    Context.withNeuronsRange (3 to 3)
    Context.withSynapsesDensity (2.5)
    Context.withInputTickMultiplierRange(2.0 <=> 2.0)

    val ng = NetGenome(netData, AccessMap(inputIds, outputIds))
    assertEquals(3, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in1"))
    assertNotEquals(None, ng.find("out1"))

    MutationsProfile(
      "deleteNeuron" -> 1.0
    ).mutate(ng)

    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in1"))
    assertNotEquals(None, ng.find("out1"))
  }

  @Test def shouldPerformRandom(): Unit ={
    var result = 0
    val f1 = () => { result = 1 }
    val f2 = () => { result = 2 }
    val f3 = () => { result = 3 }

    Probability.performRandom((1.0,f1), (0.0,f2), (0.0,f3))
    assertEquals(1, result)

    Probability.performRandom((0.0,f1), (1.0,f2), (0.0,f3))
    assertEquals(2, result)

    Probability.performRandom((0.0,f1), (0.0,f2), (1.0,f3))
    assertEquals(3, result)

    Probability.performRandom((0.0,f1), (0.0,f2), (0.0,f3))
    assertEquals(1, result)
  }

  @Test def shouldTrimMiddleNeuronFromNet(): Unit = {
    val builder = NetBuilder()
    builder.addInput("in").chain("mi11",1.0,0.0).chain("out",0.5,0.81)
    builder.addMiddle("mi12")

    val gen = NetGenome(builder.data, Map("in" -> MutationAccessInput(), "out" -> MutationAccessOutput()))
    assertEquals(4, gen.neurons.size)
    val trimmed = gen.clone()
    trimmed.trim()
    assertEquals(3, trimmed.neurons.size)
    val idSet = trimmed.neurons.map(_.id).toSet
    assertTrue(idSet.contains("in"))
    assertTrue(idSet.contains("mi11"))
    assertTrue(idSet.contains("out"))
    assertFalse(idSet.contains("mi12"))
  }

  @Test def shouldNotTrimInputNeuronFromNet(): Unit = {
    val builder = NetBuilder()
    builder.addInput("in1").chain("mi11",1.0,0.0).chain("out",0.5,0.81).addMiddle("mi12").addInput("in2")

    val gen = NetGenome(builder.data, Map("in1" -> MutationAccessInput(), "in2" -> MutationAccessInput(), "out" -> MutationAccessOutput()))
    assertEquals(5, gen.neurons.size)
    val trimmed = gen.clone()
    trimmed.trim()
    assertEquals(4, trimmed.neurons.size)
    val idSet = trimmed.neurons.map(_.id).toSet
    assertFalse(idSet.contains("mi12"))
    assertTrue(idSet.contains("in2"))
  }

  @Test def shouldNotTrimOutputNeuronFromNet(): Unit = {
    val builder = NetBuilder()
    builder.addInput("in1").chain("mi11",1.0,0.0).chain("out",0.5,0.81).addMiddle("out2")

    val gen = NetGenome(builder.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput(), "out2" -> MutationAccessOutput()))
    assertEquals(4, gen.neurons.size)
    val trimmed = gen.clone()
    trimmed.trim()
    assertEquals(4, trimmed.neurons.size)
    assertTrue(trimmed.neurons.map(_.id).toSet.contains("out2"))
  }

  private def sumSynapses(data: NetData) = {
    println("---")
    data.neurons.foreach( n => n.synapses.foreach( s => println(s"synapse from ${n.id} to ${s.neuronId}")))
    println("---")
    data.neurons.foldLeft(0)((sum, neuron) => sum + neuron.synapses.size)
  }

  private def replaceSynapses(data: NetData, neuronId: String, synapses: List[SynapseData])
  = data.neurons.find(_.id == neuronId) match {
      case Some(neuron) =>
        val synapsesChanged = neuron.withSynapses(synapses)
        data.withNeurons(synapsesChanged :: data.neurons.filterNot(_.id == neuronId))
      case None => data
    }

  @Test def shouldTrimSynapseFromNet(): Unit ={
    val builder = NetBuilder()
    builder.addInput("in").chain("mi11",1.0,0.0).chain("out",0.5,0.81)
    val netData = builder.data
    assertEquals(2, sumSynapses(netData))

    val mi11 = netData.neurons.find(_.id == "mi11").get
    assertEquals(1, mi11.synapses.size)
    // adding a synapse to non-existing neuron
    val netDataWithSynapse = replaceSynapses(netData, "mi11", SynapseData("mi12",Hush()) :: mi11.synapses)
    assertEquals(3, sumSynapses(netDataWithSynapse))

    val trimmed = NetGenome(netDataWithSynapse, Map("in" -> MutationAccessInput(), "out" -> MutationAccessOutput()))
    trimmed.trim()
    assertEquals(3, trimmed.neurons.size)
    assertEquals(2, sumSynapses(trimmed.data))
    val mi11Trimmed = trimmed.neurons.find(_.id == "mi11").get
    assertEquals(1, mi11Trimmed.synapses.size)
    assertEquals(SynapseData("out",0.5), mi11Trimmed.synapses.find(_.neuronId == "out").get.data)
    assertEquals(None, mi11Trimmed.synapses.find(_.neuronId == "mi12"))
  }

  @Test def shouldCloneGenome(): Unit ={
    val builder = NetBuilder()
    builder.addInput("in").chain("mi11",1.0,0.0).chain("out",0.5,0.81)
    val genome = NetGenome(builder.data, Map("in" -> MutationAccessInput(), "out" -> MutationAccessOutput()))
    val cloned = genome.clone
    assertEquals(genome.data, cloned.data)
    assertEquals(genome.accessMap, cloned.accessMap)
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
    val poll = GenomePoll(netData, inputIds, outputIds, 2)
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
    val poll = GenomePoll(netData, inputIds, outputIds, 1)
    val netG = poll.genomes(0)
    val net1Middle = netG.filterNot(inputIds ++ outputIds)
    val net1FullAccess = netG.fullAccessNeurons
    assertEquals(net1Middle, net1FullAccess)
  }

  @Test def shouldCreateNewGenome(): Unit ={
    val poll = GenomePoll(netData, inputIds, outputIds, 2)
    val net1G = poll.genomes(0)
    val net2G = poll.genomes(1)

    val variables = net1G.fullAccessNeurons ++ net2G.fullAccessNeurons

    val ids = variables.map(_.id).toSet
    println(s"ids: ${ids.mkString(", ")}")
    val (ids1, ids2) = Utils.splitIdsRandomly(ids, 2)
    println(s"ids1: ${ids1.mkString(", ")}")
    val newGen = NetGenome.breed(net1G, variables.filter(n => ids.contains(n.id)))

    val expectedIds = ids1.map(id => NetData.replaceNetId(id, net1G.id)) ++ net1G.notFullAccessNeurons.map(_.id).toSet
    println(s"expectedIds: ${expectedIds.mkString(", ")}")
    val newGenIds = newGen.neurons.map(_.id).toSet
    println(s"newGenIds: ${newGenIds.mkString(", ")}")

    assertEquals(expectedIds.size, newGenIds.size)
    assertFalse(expectedIds.exists(!newGenIds.contains(_)))
    assertFalse(newGenIds.exists(!expectedIds.contains(_)))
  }

  @Test def shouldCrossTwoEvenGenomes(): Unit ={
    val b1 = NetBuilder()
    b1.netId = "net1"
    b1.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    b1.use("in1").chain("net1_3",1.0,0.0).chain("net1_4",1.0,0.0).connect("out1",1.0)
    val net1G = NetGenome(b1.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput()))

    val b2 = NetBuilder()
    b2.netId = "net2"
    b2.addInput("in1").chain("net2_1",1.0,0.0).chain("net2_2",1.0,0.0).chain("out1",0.5,0.81)
    b2.use("in1").chain("net2_3",1.0,0.0).chain("net2_4",1.0,0.0).connect("out1",1.0)
    val net2G = NetGenome(b2.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput()))

    crossTest(net1G, net2G)
  }

  @Test def shouldCrossTwoUnevenGenomes(): Unit ={
    val b1 = NetBuilder()
    b1.netId = "net1"
    b1.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    val net1G = NetGenome(b1.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput()))

    val b2 = NetBuilder()
    b2.netId = "net2"
    b2.addInput("in1").chain("net2_1",1.0,0.0).chain("net2_2",1.0,0.0).chain("out1",0.5,0.81)
    b2.use("in1").chain("net2_3",1.0,0.0).chain("net2_4",1.0,0.0).connect("out1",1.0)
    val net2G = NetGenome(b2.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput()))

    crossTest(net1G, net2G)
  }

  @Test def shouldNotCrossGenomesWithoutCommonNeurons(): Unit = {
    val b1 = NetBuilder()
    b1.netId = "net1"
    b1.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    b1.use("in1").chain("net1_3",1.0,0.0).chain("net1_4",1.0,0.0).connect("out1",1.0)
    val net1G = NetGenome(b1.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput()))

    val b2 = NetBuilder()
    b2.netId = "net2"
    b2.addInput("in1").chain("net2_11",1.0,0.0).chain("net2_12",1.0,0.0).chain("out1",0.5,0.81)
    b2.use("in1").chain("net2_13",1.0,0.0).chain("net2_14",1.0,0.0).connect("out1",1.0)
    val net2G = NetGenome(b2.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput()))

    assertFalse(net1G.crossable(net2G))

    val (net12G, net21G) = net1G.cross(net2G)
    assertEquals(net1G.data, net12G.data)
    assertEquals(net2G.data, net21G.data)
  }

  @Test def shouldNotCrossGenomesWithDifferentIO(): Unit = {
    val b1 = NetBuilder()
    b1.netId = "net1"
    b1.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    b1.use("in1").chain("net1_3",1.0,0.0).chain("net1_4",1.0,0.0).connect("out1",1.0)
    val net1G = NetGenome(b1.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput()))

    val b2 = NetBuilder()
    b2.netId = "net2"
    b2.addInput("in2").chain("net2_1",1.0,0.0).chain("net2_2",1.0,0.0).chain("out2",0.5,0.81)
    b2.use("in2").chain("net2_3",1.0,0.0).chain("net2_4",1.0,0.0).connect("out2",1.0)
    val net2G = NetGenome(b2.data, Map("in2" -> MutationAccessInput(), "out2" -> MutationAccessOutput()))

    intercept[AssertionError] {
      net1G.crossable(net2G)
    }

  }

  private def crossTest(net1G: NetGenome, net2G: NetGenome): Unit ={
    val net1Middle = net1G.filterNot(inputIds ++ outputIds).map(_.id)
    assertTrue(net1Middle.nonEmpty)
    val net2Middle = net2G.filterNot(inputIds ++ outputIds).map(_.id)
    assertTrue(net2Middle.nonEmpty)

    debug("---")
    debug("net1G: " + net1G.neurons.map(_.id))
    debug("net2G: " + net2G.neurons.map(_.id))
    val (net12G, net21G) = net1G.cross(net2G)
    debug("net12G: " + net12G.neurons.map(_.id))
    debug("net21G: " + net21G.neurons.map(_.id))
    debug("---")
    // has the same input neurons
    assertEquals(net12G.inputs, inputIds)
    // has the same output neurons
    assertFalse(outputIds.exists( net12G.find(_).isEmpty ))
    // has non-empty middle layer
    val net12Middle = net12G.filterNot(inputIds ++ outputIds).map(_.id)
    assertTrue(net12Middle.nonEmpty)

    // has the same input neurons
    assertEquals(net21G.inputs, inputIds)
    // has the same output neurons
    assertFalse(outputIds.exists( net21G.find(_).isEmpty ))
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

  @Test def shouldCrossResultInWorkingNet(): Unit ={
    val b1 = NetBuilder()
    b1.netId = "net1"
    b1.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    b1.use("in1").chain("net1_3",1.0,0.0).chain("net1_4",1.0,0.0).connect("out1",1.0)
    val net1G = NetGenome(b1.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput()))

    val b2 = NetBuilder()
    b2.netId = "net2"
    b2.addInput("in1").chain("net2_11",1.0,0.0).chain("net2_12",1.0,0.0).chain("out1",0.5,0.81)
    b2.use("in1").chain("net2_13",1.0,0.0).chain("net2_14",1.0,0.0).connect("out1",1.0)
    val net2G = NetGenome(b2.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput()))

    assertFalse(net1G.crossable(net2G))

    val (net12G, net21G) = net1G.cross(net2G)
    val b3 = NetBuilder(net12G.data)
    val wrapper = b3.build()
    assertNotNull(wrapper)
  }

  @Test def shouldCrossResultInWorkingNetWithTrimming(): Unit ={
    val b1 = NetBuilder()
    b1.netId = "net1"
    b1.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    b1.use("in1").chain("net1_3",1.0,0.0)
    val net1G = NetGenome(b1.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput()))

    val b2 = NetBuilder()
    b2.netId = "net2"
    b2.addInput("in1").chain("net2_12",1.0,0.0)
    b2.use("in1").chain("net2_13",1.0,0.0).chain("net2_14",1.0,0.0).chain("out1",1.0)
    val net2G = NetGenome(b2.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput()))

    assertFalse(net1G.crossable(net2G))

    val (net12G, net21G) = net1G.cross(net2G)
    val b3 = NetBuilder(net12G.data)
    val wrapper = b3.build()
    assertNotNull(wrapper)
  }

  @Test def shouldDeleteNeuronFromGenome(): Unit ={
    val b1 = NetBuilder()
    b1.netId = "net1"
    b1.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    b1.use("in1").chain("net1_3",1.0,0.0).connect("out1",0.81)
    val net1G = NetGenome(b1.data, Map("in1" -> MutationAccessInput(), "out1" -> MutationAccessOutput()))

    net1G.deleteNeuron("net1_3")
    intercept[AssertionError] {
      net1G.data.validate()
    }

    net1G.deleteSynapsesTo("net1_3")
    net1G.data.validate()
  }

  @Test def shouldRename(): Unit ={
    val data = NetBuilder().addInput("in").chain("mi1").chain("mi2").chain("out").netId("net").data
    val genome = NetGenome(data, AccessMap("in","out"))

    assertTrue(data.contains("in"))
    assertTrue(data.contains("mi1"))
    assertTrue(data.contains("mi2"))
    assertTrue(data.contains("out"))
    assertEquals("net",genome.id)

    genome.netId("newname")

    assertEquals("newname",genome.id)
    assertTrue(genome.data.contains("in"))
    assertTrue(genome.data.contains("newname_mi1"))
    assertTrue(genome.data.contains("newname_mi2"))
    assertTrue(genome.data.contains("out"))

    genome.netId("newername")

    assertEquals("newername",genome.id)
    assertTrue(genome.data.contains("in"))
    assertTrue(genome.data.contains("newername_mi1"))
    assertTrue(genome.data.contains("newername_mi2"))
    assertTrue(genome.data.contains("out"))
    assertFalse(genome.data.contains("newname_mi1"))
    assertFalse(genome.data.contains("newname_mi2"))
  }
}
