package test


import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import main._

class DelayRes4Suite extends JUnitSuite {
  private def dotNetRes4() = {
    val builder = NetBuilder()
    builder.middleNeuronType = NeuronType.DELAY
    builder.defSlope = 5.0
    builder.addInput("in1")
    // dots
    builder.use("in1").chainMiddle("mi11",0.6,0.5).loop("loop1",1.0,0.5,1.0).chainMiddle("mi12",1.0,0.75).chainOutput("out1",1.0)
    builder.use("out1").connect("mi11", -0.49)
    builder.use("out1").connect("mi12", -1.0)
    
    val (in, net, out) = builder.build("in","out",4)
    val out1 = builder.get("out1")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(out1, (n:Neuron) => {
      println("KROPA!")
      sb.append('.'); 
    })
    
    (in, sb)
  }
  
  @Test
  def shouldDotThenNothingRes4(){
    val (in, sb) = dotNetRes4
    
    in += "1,0,0,0,0,0"
    val interval = in.tickUntilCalm()
    assertEquals(".",sb.toString)
    println(s"interval: $interval")
  }
  
  @Test
  def shouldDot3TimesRes4(){
    val (in, sb) = dotNetRes4
    
    in += "1,0,0,1,0,0,1,0,0"
    val interval = in.tickUntilCalm()
    assertEquals("...",sb.toString)
    println(s"interval: $interval")
  }
  
  private def lineNetRes4() = {
    val builder = NetBuilder()
    builder.middleNeuronType = NeuronType.DELAY
    builder.defSlope = 5.0
    // lines
    builder.addInput("in1").chainMiddle("mi21",0.4,0.65).loop("loop2",1.0,0.5,1.0).chainMiddle("mi22",1.0,0.9).chainOutput("out2",1.0)
    builder.use("mi21").setForgetting(0.1)
    builder.use("mi22").connect("mi21", -0.35)
    
    val (in, net, out) = builder.build("in","out",4)
    val out2 = builder.get("out2")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(out2, (n:Neuron) => {
      println("KRECHA!")
      sb.append('-'); 
    })
    
    (in, sb, net)
  }
  
  @Test
  def shouldLineThenNothingRes4(){
    val (in, sb, net) = lineNetRes4
    
    val netStateLog = StringBuilder.newBuilder
    val neurons = net.size

    net.addAfterTickTrigger( net => {
      val outputSum = net.outputSum
      val weightSum = net.weightSum
      val iteration = net.iteration
      netStateLog.append(s"#${iteration}: outputSum=${outputSum}, avgOutput=${outputSum/neurons}\n")
    })
    
    in += "1,1,0,0,0,0"
    in.tickUntilCalm()
    println( netStateLog.toString )
    
    net.getNeurons.foreach( n => {
      println(n.id + ": " + n.getClass().getName())
    })
    assertEquals("-",sb.toString)
  }
  
  @Test
  def shouldLine3TimesRes4(){
    val (in, sb, _) = lineNetRes4
    
    in += "1,1,0,1,1,0,1,1,0"
    in.tickUntilCalm()
    assertEquals("---",sb.toString)
  }
    
  @Test
  def shouldLine2TimesWithSpaceRes4(){
    val (in, sb, _) = lineNetRes4
    
    in += "1,1,0,0,1,1,0,0"
    in.tick(12)
    assertEquals("--",sb.toString)
  }
  
  @Test
  def shouldNotLineRes4(){
    val (in, sb, _) = lineNetRes4
    
    in += "1,0,0,1,0,0"
    in.tickUntilCalm()
    assertEquals("",sb.toString)
  }
}