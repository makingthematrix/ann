package test

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import main._

class NetBuilderSuite extends JUnitSuite {
  private def buildNet() = {
    val builder = NetBuilder()
    builder.middleNeuronType = NeuronType.DELAY
    builder.addInput("in1").chainMiddle("mi21",0.4,0.65).loop("loop2",1.0,0.5,1.0).chainMiddle("mi22",1.0,0.9).chainOutput("out2",1.0)

    val (in, net, out) = builder.build("in","out")
 
    net
  }
  
  @Test
  def shouldUseDelayNeurons(){
    val net = buildNet
    net.getNeurons.foreach( n => n.id match{
      case "in1" => assertEquals(s"${n.id} main.DummyNeuron",n.id + " " + n.getClass().getName())
      case "mi21" => assertEquals(s"${n.id} main.DelayNeuron",n.id + " " + n.getClass().getName())
      case "loop2" => assertEquals(s"${n.id} main.DelayNeuron",n.id + " " + n.getClass().getName())
      case "mi22" => assertEquals(s"${n.id} main.DelayNeuron",n.id + " " + n.getClass().getName())
      case "out2" => assertEquals(s"${n.id} main.DummyNeuron",n.id + " " + n.getClass().getName())
    })
    
    net.getNeurons.foreach( n => {
      println(n.id + ": " + n.getClass().getName())
    })
  }
}