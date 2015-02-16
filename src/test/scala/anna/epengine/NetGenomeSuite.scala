package anna.epengine

import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import anna.utils.DoubleRange._

/**
 * Created by gorywoda on 16.02.15.
 */
class NetGenomeSuite extends JUnitSuite {
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
}
