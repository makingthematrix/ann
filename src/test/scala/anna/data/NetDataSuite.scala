package anna.data

import anna.async.NetBuilderOps._
import anna.async.{NetBuilder, NeuronTypeDummy, NeuronTypeStandard}
import anna.logger.LOG
import org.junit.Assert._
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite

/**
 * Created by gorywoda on 03.01.15.
 */
class NetDataSuite extends JUnitSuite {
  @Before def before(): Unit = {
    LOG.addLogToStdout()
  }

  @Test def shouldMakeNetDataFromJson() = {
    val s1 = SynapseData("id2",1.0)
    val n1 = NeuronData("id1",0.0, 1, List(s1), NeuronTypeDummy())
    val n2 = NeuronData("id2",0.0, 2, Nil)
    val netData = NetData("net",List(n1,n2),List("id1"))

    val json = netData.toJson
    assertEquals(netData, NetData.fromJson(json))
  }

  @Test def shouldMakeNetDataWithBuilder() = {
    val s1 = SynapseData("id2",1.0)
    val n1 = NeuronData("id1", 1).withSynapses(List(s1))
    val n2 = NeuronData("id2",0.0, 2, Nil)
    val netData = NetData("net",List(n1,n2),List("id1"))

    val builder = NetBuilder()

    builder.addInput("id1").chain("id2", 1.0, 0.0, 2)

    print("---- net data ----")
    print(netData.toJson)
    print("---- builder ----")
    print(builder.data.toJson)

    assertEquals(netData.toJson, builder.data.toJson)
  }

  @Test def shouldBuildNetWithData() = {
    val s1 = SynapseData("id2",1.0)
    val n1 = NeuronData("id1", 0.0, 1, List(s1), NeuronTypeDummy())
    val n2 = NeuronData("id2", 0.0, 2, Nil, NeuronTypeStandard())
    val netData = NetData("net",List(n1,n2),List("id1"))

    val builder = NetBuilder()
    builder.set(netData)

    val netWrapper = builder.build("in")
    val neurons = netWrapper.net.getNeurons
    assertEquals(2, neurons.size)
    assertEquals(List("id1","id2"), neurons.map(_.id).sorted)

    val sb = StringBuilder.newBuilder
    netWrapper.addAfterFire("id2")( (_:Double)=>{ sb.append(".") } )

    netWrapper += "1,1,1"

    netWrapper.tick(20)

    assertEquals("...",sb.toString)
  }

  private def SOSNetWithHushNeuron(builder: NetBuilder){
    builder.addInput("in")
    // dots
    builder.use("in").chain("mi11",1.0, 0.0, 2).silence("mi11")
      .chain("mi12",1.0,0.0).loop("loop",1.0,0.0,1.0)
      .chain("dot",0.6/2.0,0.6)
      .chain("S",0.5,0.81)
    builder.addSilencingNeuron("dot_silence").silence("mi12").silence("loop").silence("dot")
    builder.use("dot").silence("dot_silence")

    // lines
    builder.use("in").chain("mi21",0.55,0.58).silence("mi21")
      .chain("line",1.0,0.0).silence("line")
      .chain("O",0.6,0.81)

    // if line then not dot
    builder.use("line").silence("dot_silence")

    // if S then not O, if O then not S...
    builder.use("S").chainSilencingNeuron("hush_letters").silence("S").silence("O")
    builder.use("O").silence("hush_letters")
  }

}
