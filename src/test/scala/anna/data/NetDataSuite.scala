package anna.data

import anna.async.{NetBuilder, NeuronType}
import anna.logger.LOG
import org.junit.Assert._
import org.junit.{Test, Before}
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
    val n1 = NeuronData("id1",0.0,5.0,HushValue(1),DontForget, List(s1), 1.0, NeuronType.DUMMY)
    val n2 = NeuronData("id2",0.0,5.0,HushValue(2),ForgetValue(0.4), 1.0)
    val netData = NetData("net",List(n1,n2),List("id1"))

    val json = netData.toJson
    assertEquals(netData, NetData.fromJson(json))
  }

  @Test def shouldMakeNetDataWithBuilder() = {
    val s1 = SynapseData("id2",1.0)
    val n1 = NeuronData("id1",HushValue(1),1.0)
    val n2 = NeuronData("id2",0.0,5.0,HushValue(2),ForgetValue(0.4), 1.0)
    val netData = NetData("net",List(n1,n2),List("id1"))

    val builder = NetBuilder()

    builder.addInput("id1", 1.0).chain("id2",1.0,0.0,5.0,HushValue(2),ForgetValue(0.4),1.0)
    assertEquals(netData, builder.data)
  }

  @Test def shouldBuildNetWithData() = {
    val s1 = SynapseData("id2",1.0)
    val n1 = NeuronData("id1", 0.0, 5.0, HushValue(1), ForgetAll, List(s1), 1.0, NeuronType.DUMMY)
    val n2 = NeuronData("id2", 0.0, 5.0, HushValue(2), ForgetValue(0.4), Nil, 1.0, NeuronType.STANDARD)
    val netData = NetData("net",List(n1,n2),List("id1"))

    val builder = NetBuilder()
    builder.set(netData)

    val (in, net) = builder.build("in")
    val neurons = net.getNeurons
    assertEquals(2, neurons.size)
    assertEquals(List("id1","id2"), neurons.map(_.id).sorted)

    val sb = StringBuilder.newBuilder
    net.addAfterFire("id2"){ sb.append(".") }

    in += "1,1,1"

    in.tick(7)

    assertEquals("...",sb.toString)
  }
}
