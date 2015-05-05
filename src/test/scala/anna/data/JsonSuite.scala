package anna.data

import anna.async.NeuronType
import anna.logger.LOG
import org.junit.Assert._
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite

/**
 * Created by gorywoda on 27.12.14.
 */
class JsonSuite extends JUnitSuite {
  @Before def before(): Unit = {
    LOG.addLogToStdout()
  }

  @Test def shouldMakeJsonFromSynapseData() = {
    val data1 = SynapseData("id1",1.0)
    assertEquals("{\"neuronId\":\"id1\",\"weight\":\"SynapseWeight(1.0)\"}", data1.toJson)

    val data2 = SynapseData("id2",Hush())
    assertEquals("{\"neuronId\":\"id2\",\"weight\":\"Hush\"}", data2.toJson)

    val data3 = SynapseData("id3",-1.0)
    assertEquals("{\"neuronId\":\"id3\",\"weight\":\"SynapseWeight(-1.0)\"}", data3.toJson)
  }

  @Test def shouldMakeSynapseDataFromJson() = {
    val json1 = "{\"neuronId\":\"id1\",\"weight\":\"SynapseWeight(1.0)\"}"
    val data1 = SynapseData.fromJson(json1)
    assertEquals(SynapseData("id1",1.0), data1)

    val json2 = "{\"neuronId\":\"id2\",\"weight\":\"Hush\"}"
    val data2 = SynapseData.fromJson(json2)
    assertEquals(SynapseData("id2",Hush()), data2)

    val json3 = "{\"neuronId\":\"id3\",\"weight\":\"SynapseWeight(-1.0)\"}"
    val data3 = SynapseData.fromJson(json3)
    assertEquals(SynapseData("id3",-1.0), data3)

    val d4 = SynapseData("id1",1.0)
    val json4 = d4.toJson
    assertEquals(d4, SynapseData.fromJson(json4))
  }

  @Test def shouldMakeJsonFromNeuronData() = {
    val data1 = NeuronData("id1",0.0,5.0,HushValue(1),DontForget, 1.0)
    assertEquals("{\"id\":\"id1\",\"threshold\":0.0,\"slope\":5.0,\"hushValue\":\"HushValue(1)\",\"forgetting\":\"DontForget\",\"synapses\":[],\"tickTimeMultiplier\":1.0,\"neuronType\":\"STANDARD\"}", data1.toJson)
  }

  @Test def shouldMakeNeuronDataFromJson() = {
    val data1 = NeuronData("id1",0.0,5.0,HushValue(1),DontForget, 1.0)
    val json1 = data1.toJson
    assertEquals(data1, NeuronData.fromJson(json1))

    val data2 = NeuronData("id1",0.0,5.0,HushValue(2),ForgetAll, List(SynapseData("id2",Hush()),SynapseData("id3",1.0)), 2.0)
    val json2 = data2.toJson
    assertEquals(data2, NeuronData.fromJson(json2))
  }
}
