package anna.data

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
    val expectedJson1 =
      """
        |{
        |  "neuronId":"id1",
        |  "weight":{
        |    "jsonClass":"SynapseWeight",
        |    "weight":1.0
        |  }
        |}
      """.stripMargin.trim
    val data1 = SynapseData("id1",1.0)
    assertEquals(expectedJson1, data1.toJson)

    val expectedJson2 =
      """
        |{
        |  "neuronId":"id2",
        |  "weight":{
        |    "jsonClass":"Silence"
        |  }
        |}
      """.stripMargin.trim
    val data2 = SynapseData("id2",Silence())
    assertEquals(expectedJson2, data2.toJson)
  }

  @Test def shouldMakeSynapseDataFromJson() = {
    val json1 =
      """
        |{
        |  "neuronId":"id1",
        |  "weight":{
        |    "jsonClass":"SynapseWeight",
        |    "weight":1.0
        |  }
        |}
      """.stripMargin.trim
    val data1 = SynapseData.fromJson(json1)
    assertEquals(SynapseData("id1",1.0), data1)

    val json2 =
      """
        |{
        |  "neuronId":"id2",
        |  "weight":{
        |    "jsonClass":"Silence"
        |  }
        |}
      """.stripMargin.trim
    val data2 = SynapseData.fromJson(json2)
    assertEquals(SynapseData("id2",Silence()), data2)

    val d4 = SynapseData("id1",1.0)
    val json4 = d4.toJson
    assertEquals(d4, SynapseData.fromJson(json4))
  }

  @Test def shouldMakeJsonFromNeuronData() = {
    val n1 = NeuronData("id1",0.0,SilenceIterations(1), Nil)
    val json = n1.toJson
    val n2 = NeuronData.fromJson(json)
    assertEquals(n1, n2)
  }

}
