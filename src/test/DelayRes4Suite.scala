package test


import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import main._

class DelayRes4Suite extends JUnitSuite {
  private def netRes4() = {
    val builder = NetBuilder()
    builder.middleNeuronType = NeuronType.DELAY
    builder.defSlope = 5.0
    builder.defForgetting = 0.1
    builder.addInput("in1")
    // dots
    //builder.use("in1").chainMiddle("mi11",0.24,0.5).loop("loop1",1.0,0.5,1.0).chainMiddle("mi12",1.0,0.75).chainOutput("out1",1.0)
    builder.use("in1").chainMiddle("mi11",0.28,0.5).loop("loop1",1.0,0.5,1.0).chainMiddle("mi12",1.0,0.9).chainOutput("out1",1.0)
    builder.use("mi11").setForgetting(0.2)
    builder.use("mi12").setForgetting(0.2)
    builder.use("mi12").connect("mi11", -0.49)
    builder.use("mi12").connect("loop1", -1.0)
    builder.use("out1").connect("mi12", -1.0)
    // lines
    //builder.use("in1").chainMiddle("mi21",0.18,0.5).loop("loop2",0.95,0.5,1.0).chainMiddle("mi22",1.0,0.6).chainOutput("out2",1.0)
    builder.use("in1").chainMiddle("mi21",0.19,0.5).chainMiddle("mi22",1.0,0.5).chainOutput("out2",1.0)
    builder.use("mi22").connect("mi21", -0.35)
    // if line then not dot
    builder.use("mi21").connect("mi11", -1.0)
    builder.use("mi21").connect("loop1", -1.0)
    builder.use("mi22").connect("mi11", -1.0)
    builder.use("mi22").connect("loop1", -1.0)
    
    val (in, net, out) = builder.build("in","out",4)
    val out1 = builder.get("out1")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(out1, (n:Neuron) => {
      println("KROPA!")
      sb.append('.'); 
    })
    val out2 = builder.get("out2")
    out.addAfterFireTrigger(out2, (n:Neuron) => {
      println("KRECHA!")
      sb.append('-'); 
    })
    
    (in, sb, net)
  }
  
  @Test
  def shouldDotThenNothingRes4(){
    val (in, sb, _) = netRes4
    
    in += "1,0,0,0,0,0"
    val interval = in.tickUntilCalm()
    assertEquals(".",sb.toString)
    println(s"interval: $interval")
  }
  
  @Test
  def shouldDot3TimesRes4(){
    val (in, sb, _) = netRes4
    
    in += "1,0,0,1,0,0,1,0,0"
    val interval = in.tickUntilCalm()
    assertEquals("...",sb.toString)
    println(s"interval: $interval")
  }
  
  @Test
  def shouldLineThenNothingRes4(){
    val (in, sb, net) = netRes4
    
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
    val (in, sb, _) = netRes4
    
    in += "1,1,0,1,1,0,1,1,0"
    in.tickUntilCalm()
    assertEquals("---",sb.toString)
  }
    
  @Test
  def shouldLine2TimesWithSpaceRes4(){
    val (in, sb, _) = netRes4
    
    in += "1,1,0,0,1,1,0,0"
    in.tick(12)
    assertEquals("--",sb.toString)
  }
  
  @Test
  def shouldDot2NotLineRes4(){
    val (in, sb, _) = netRes4
    
    in += "1,0,0,1,0,0"
    in.tickUntilCalm()
    assertEquals("..",sb.toString)
  }
  
    @Test
  def shouldDotNotLine1(){
	val (in, sb, _) = netRes4
	
	in += "1,0,0,0,0,0"
	in.tickUntilCalm()
	assertEquals(".",sb.toString)
  }
  
  
  @Test
  def shouldDotNotLine3(){
	val (in, sb, _) = netRes4
	
	in += "1,0,0,1,0,0,1,0,0"
	in.tickUntilCalm()
	assertEquals("...",sb.toString)
  }
  
  @Test
  def shouldLineNotDot1(){
	val (in, sb, _) = netRes4
	LOG.allow("mi11","mi21")
	in += "1,1,0,0,0,0"
	in.tickUntilCalm()
	assertEquals("-",sb.toString)
	LOG.clearAllowedIds()
  }
  
  @Test
  def shouldLineNotDot2(){
	val (in, sb, _) = netRes4
	
	in += "1,1,0,1,1,0"
	in.tickUntilCalm()
	assertEquals("--",sb.toString)
  }
  
  @Test
  def shouldLineNotDot3(){
	val (in, sb, _) = netRes4
	
	in += "1,1,0,1,1,0,1,1,0"
	in.tickUntilCalm()
	assertEquals("---",sb.toString)
  }
  
  @Test
  def shouldDotThenLine(){
	val (in, sb, _) = netRes4
	LOG.allow("mi21","mi11","loop1")
	in += "1,0,0,1,1,0"
	in.tickUntilCalm()
	assertEquals(".-",sb.toString)
	LOG.clearAllowedIds()
  }
  
  @Test
  def shouldDotThenLineThenDot_separate(){
	val (in, sb, _) = netRes4
	
	in += "1,0,0"
	val interval1 = in.tickUntilCalm()
	assertEquals(".",sb.toString)
	in += "1,1,0"
	val interval2 = in.tickUntilCalm()
	assertEquals(".-",sb.toString)
	in += "1,0,0"
	val interval3 = in.tickUntilCalm()
	assertEquals(".-.",sb.toString)
	println(s"intervals: $interval1, $interval2, $interval3")
  }
  
    @Test
  def shouldDotThenLine_together(){
	val (in, sb, _) = netRes4
	LOG.allow("mi21","mi11","loop1")
	in += "1,0,0,1,1,0,1,0,0"
	in.tickUntilCalm()
	assertEquals(".-.",sb.toString)
	LOG.clearAllowedIds()
  }
}