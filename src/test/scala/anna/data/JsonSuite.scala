package anna.data

import org.junit.{After, Before, Test}
import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import anna.async.logger.LOG
import anna.async.logger.LOG._

/**
 * Created by gorywoda on 27.12.14.
 */
class JsonSuite extends JUnitSuite {
  @Before def before(): Unit ={
    LOG.addLogToStdout()
  }

  @After def after(): Unit ={
    LOG.resetOuts()
  }

  @Test def shouldMakeJsonFromSynapseData() = {
    val data1 = SynapseData("id1",1.0)
    assertEquals("{\"neuronId\":\"id1\",\"weight\":\"SynapseWeight(1.0)\"}", data1.toJson)

    val data2 = SynapseData("id2",Hush)
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
    assertEquals(SynapseData("id2",Hush), data2)

    val json3 = "{\"neuronId\":\"id3\",\"weight\":\"SynapseWeight(-1.0)\"}"
    val data3 = SynapseData.fromJson(json3)
    assertEquals(SynapseData("id3",-1.0), data3)
  }
}
