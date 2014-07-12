package test

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import main._
import main.logger.LOG
import main.NetBuilderOps._

class SOSSuite extends JUnitSuite {
  
  private lazy val sosNet = {
    val builder = NetBuilder(NeuronType.DELAY, 5.0, 0.1, 4)

    builder.addInput("in1")
   
    builder.dotLine("in1", "dot", "line")
    
    builder.use("dot").chainMiddle("S",0.5,0.9).setForgetting(0.01)
    builder.use("line").chainMiddle("O",0.9,0.9).setForgetting(0.01)
    
    val (in, net, out) = builder.build("in","out")
    out.addAfterFireTrigger(builder.get("dot"), (_:NeuronLike) => println("KROPA!") )
    out.addAfterFireTrigger(builder.get("line"), (_:NeuronLike) => println("KRECHA!") )

    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(builder.get("S"), (n:NeuronLike) => {
      println("S!")
      sb.append('S'); 
    })
    out.addAfterFireTrigger(builder.get("O"), (n:NeuronLike) => {
      println("O!")
      sb.append('O'); 
    })
    
    (in, sb, net)
  }
  
  
  val s = "1,0,0,1,0,0,1,0,0"
  val o = "1,1,0,1,1,0,1,1,0"
      
  @Test
  def shouldReturnS() = {
    val (in, sb, _) = sosNet
    LOG.allow("dot","S")
    in += s
    val interval = in.tickUntilCalm()
    assertEquals("S",sb.toString)
    println(s"interval: $interval")
    LOG.clearAllowedIds()
  }
  
  @Test
  def shouldReturnO() = {
    val (in, sb, _) = sosNet
    LOG.allow("line","O")
    in += o
    val interval = in.tickUntilCalm()
    assertEquals("O",sb.toString)
    println(s"interval: $interval")
    LOG.clearAllowedIds()
  }
  
  @Test
  def shouldReturnSOS() = {
    val (in, sb, _) = sosNet
    in += s
    in += o
    in += s
    val interval = in.tickUntilCalm()
    assertEquals("SOS",sb.toString)
    println(s"interval: $interval")

  }
}