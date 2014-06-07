package test


import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import main._

class LineNetSuite extends JUnitSuite {
  private def lineNet() = {
    val builder = NetBuilder()
    builder.middleNeuronType = NeuronType.DELAY
    builder.defSlope = 5.0
    builder.defForgetting = 0.1
    // lines
    builder.addInput("in1").chainMiddle("mi21",0.4,0.5).loop("loop2",0.9,0.5,1.0).chainMiddle("mi22",1.0,0.6).chainOutput("out2",1.0)
    builder.use("mi22").connect("mi21", -0.35)
    
    val (in, net, out) = builder.build("in","out")
    val out2 = builder.get("out2")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(out2, (n:Neuron) => {
      println("KRECHA!")
      sb.append('-'); 
    })
    
    (in, sb)
  }
  
  private def lineNetRes4_standard() = {
    val builder = NetBuilder()
    builder.middleNeuronType = NeuronType.DELAY
    builder.defSlope = 5.0
    builder.defForgetting = 0.1
    // lines
    builder.addInput("in1").chainMiddle("mi21",0.18,0.5).loop("loop2",0.95,0.5,1.0).chainMiddle("mi22",1.0,0.6).chainOutput("out2",1.0)
    builder.use("mi22").connect("mi21", -0.35)
    
    val (in, net, out) = builder.build("in","out",4)
    val out2 = builder.get("out2")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(out2, (n:Neuron) => {
      println("KRECHA!")
      sb.append('-'); 
    })
    
    (in, sb)
  }
  
  private def lineNetRes4() = {
    val builder = NetBuilder()
    builder.middleNeuronType = NeuronType.DELAY
    builder.defSlope = 5.0
    builder.defForgetting = 0.1
    // lines
    builder.addInput("in1").chainMiddle("mi21",0.19,0.5).chainMiddle("mi22",1.0,0.5).chainOutput("out2",1.0)
    builder.use("mi22").connect("mi21", -0.35)
    
    val (in, net, out) = builder.build("in","out",4)
    val out2 = builder.get("out2")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(out2, (n:Neuron) => {
      println("KRECHA!")
      sb.append('-'); 
    })
    
    (in, sb)
  }

  @Test
  def shouldLineThenNothing1(){
    val (in, sb) = lineNet
    
    in += "1,1,0,0,0,0"
    in.tickUntilCalm()
    assertEquals("-",sb.toString)
  }
  
  @Test
  def shouldLine3Times(){
    val (in, sb) = lineNet
    
    in += "1,1,0,1,1,0,1,1,0"
    in.tickUntilCalm()
    assertEquals("---",sb.toString)
  }
    
  @Test
  def shouldLine2TimesWithSpace(){
    val (in, sb) = lineNet
    
    in += "1,1,0,0,1,1,0,0"
    in.tick(12)
    assertEquals("--",sb.toString)
  }
  
  @Test
  def shouldNotLine(){
    val (in, sb) = lineNet
    
    in += "1,0,0,1,0,0"
    in.tickUntilCalm()
    assertEquals("",sb.toString)
  }
  
  @Test
  def shouldLineThenNothing1Res4(){
    val (in, sb) = lineNetRes4
    LOG.allow("mi21")
    in += "1,1,0,0,0,0"
    in.tickUntilCalm()
    assertEquals("-",sb.toString)
    LOG.clearAllowedIds()
  }
  
  @Test
  def shouldLine3TimesRes4(){
    val (in, sb) = lineNetRes4
    LOG.allow("mi21")
    in += "1,1,0,1,1,0,1,1,0"
    in.tickUntilCalm()
    assertEquals("---",sb.toString)
    LOG.clearAllowedIds()
  }
    
  @Test
  def shouldLine2TimesWithSpaceRes4(){
    val (in, sb) = lineNetRes4
    
    in += "1,1,0,0,1,1,0,0"
    in.tick(12)
    assertEquals("--",sb.toString)
  }
  
  @Test
  def shouldNotLineRes4(){
    val (in, sb) = lineNetRes4
    
    in += "1,0,0,1,0,0"
    in.tickUntilCalm()
    assertEquals("",sb.toString)
  }

}