package anna.epengine

import anna.async.NetBuilder
import anna.data.{NetData, SynapseData, Hush}
import anna.logger.LOG
import org.junit.Assert._
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite
import anna.utils.DoubleRange._
import anna.async.NetBuilderOps._
import anna.epengine.MutationAccess._

/**
 * Created by gorywoda on 16.02.15.
 */
class NetGenomeSuite extends JUnitSuite {
  @Before def before() {
    LOG.addLogToStdout()
  }

  @Test def shouldAddNeuron(): Unit ={
    NetGenome.neuronsRange = 2 to 2
    NetGenome.synapsesDensity = 2.5
    NetGenome.inputTickMultiplierRange = 2.0 <=> 2.0

    val inputIds = List("in1")
    val outputIds = List("out1")

    val ng = NetGenome.toss("net", inputIds, outputIds)
    assertEquals(2, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in1"))
    assertNotEquals(None, ng.find("out1"))

    NetGenome.addNeuronProbability = 1.0
    NetGenome.deleteNeuronProbability = 0.0
    NetGenome.mutateNeuronProbability = 0.0
    NetGenome.inputTickMultiplierProbability = 0.0

    ng.mutate()

    val middle1Opt = ng.find("net2")
    assertNotEquals(None, middle1Opt)
    assertTrue(ng.findSynapse("in1","net2") != None || ng.findSynapse("out1","net2") != None)
    assertNotEquals(None, ng.findSynapse("net2","out1"))
  }

  @Test def shouldDeleteNeuron(): Unit ={
    NetGenome.neuronsRange = 3 to 3
    NetGenome.synapsesDensity = 2.5
    NetGenome.inputTickMultiplierRange = 2.0 <=> 2.0

    val inputIds = List("in1")
    val outputIds = List("out1")

    val ng = NetGenome.toss("net", inputIds, outputIds)
    assertEquals(3, ng.data.neurons.size)
    assertNotEquals(None, ng.find("in1"))
    assertNotEquals(None, ng.find("out1"))

    NetGenome.addNeuronProbability = 0.0
    NetGenome.deleteNeuronProbability = 1.0
    NetGenome.mutateNeuronProbability = 0.0
    NetGenome.inputTickMultiplierProbability = 0.0

    ng.mutate()

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

    val gen = NetGenome(builder.data, Map("in" -> DONTMUTATE, "out" -> DONTDELETE))
    assertEquals(4, gen.neurons.size)
    val trimmed = gen.trim()
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

    val gen = NetGenome(builder.data, Map("in1" -> DONTMUTATE, "in2" -> DONTMUTATE, "out" -> DONTDELETE))
    assertEquals(5, gen.neurons.size)
    val trimmed = gen.trim()
    assertEquals(4, trimmed.neurons.size)
    val idSet = trimmed.neurons.map(_.id).toSet
    assertFalse(idSet.contains("mi12"))
    assertTrue(idSet.contains("in2"))
  }

  @Test def shouldNotTrimOutputNeuronFromNet(): Unit = {
    val builder = NetBuilder()
    builder.addInput("in1").chain("mi11",1.0,0.0).chain("out",0.5,0.81).addMiddle("out2")

    val gen = NetGenome(builder.data, Map("in1" -> DONTMUTATE, "out1" -> DONTDELETE, "out2" -> DONTDELETE))
    assertEquals(4, gen.neurons.size)
    val trimmed = gen.trim()
    assertEquals(4, trimmed.neurons.size)
    assertTrue(trimmed.neurons.map(_.id).toSet.contains("out2"))
  }

  private def sumSynapses(data: NetData) = data.neurons.foldLeft(0)((sum, neuron) => sum + neuron.synapses.size)

  @Test def shouldTrimSynapseFromNet(): Unit ={
    val builder = NetBuilder()
    builder.addInput("in").chain("mi11",1.0,0.0).chain("out",0.5,0.81)
    val netData = builder.data
    assertEquals(2, sumSynapses(netData))

    val mi11 = netData.neurons.find(_.id == "mi11").get
    assertEquals(1, mi11.synapses.size)
    // adding a synapse to non-existing neuron
    val mi11WithSynapse = mi11.withSynapses(SynapseData("mi12",Hush) :: mi11.synapses)
    assertEquals(2, mi11WithSynapse.synapses.size)
    val netDataWithSynapse = netData.withNeurons(mi11WithSynapse :: netData.neurons.filterNot(_.id == "mi11"))
    assertEquals(3, sumSynapses(netDataWithSynapse))

    val trimmed = NetGenome(netDataWithSynapse, Map("in" -> DONTMUTATE, "out" -> DONTDELETE))
    assertEquals(2, sumSynapses(trimmed.data))
    val mi11Trimmed = trimmed.neurons.find(_.id == "mi11").get
    assertEquals(1, mi11Trimmed.synapses.size)
    assertEquals(Some(SynapseData("out",0.5)), mi11Trimmed.synapses.find(_.neuronId == "out"))
    assertEquals(None, mi11Trimmed.synapses.find(_.neuronId == "mi12"))
  }
}
