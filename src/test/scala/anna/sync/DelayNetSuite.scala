package test.sync

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import anna._
import anna.sync._

class DelayNetSuite extends JUnitSuite {
  private def assertOutputAfter(in: NetInput, net: Net, out: NetOutput, iterations: Int) = {
    val outId:String = out.ids(0)
    
    var outputRegistered = false
    out.addAfterFireTrigger(outId, () => {
      println(s"fired!, outId=$outId, net tick=${net.iteration}")
      outputRegistered = true
    })
    
    while(!outputRegistered && net.iteration < 100) in.tick()
    
    assertTrue(outputRegistered)
    assertEquals(iterations, net.iteration)
  }
    
  @Test
  def shouldSendOutputWith3IterDelay_usingInputSynapse(){
    val builder = NetBuilder()
    builder.middleNeuronType = NeuronType.DELAY
    builder.addInput().chainMiddle(0.501,0.5).loop(1.0,0.5,1.0).chainOutput(1.0,0.75)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,0,0"
      
    assertOutputAfter(in, net, out, 8)
  }
    
  @Test
  def shouldSendOutputWith3IterDelay_usingSlopeAndSelf(){
    val builder = NetBuilder()
    builder.middleNeuronType = NeuronType.DELAY
    builder.addInput().chainMiddle(0.55,0.5,8.0).self(1.0).chainOutput(1.0,0.75)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,0,0"
      
    assertOutputAfter(in, net, out, 4)
  }
  
  @Test
  def shouldSendOutputWith2Signals_usingTreshold(){
    val builder = NetBuilder()
    builder.middleNeuronType = NeuronType.DELAY
    builder.addInput().chainMiddle(0.4,0.75,5.0).loop(1.0,0.5,1.0).chainOutput(1.0,0.9)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,1,0"
      
    assertOutputAfter(in, net, out, 6)
  }
  
  private def dotLineNet() = {
    val builder = NetBuilder()
    builder.middleNeuronType = NeuronType.DELAY
    builder.defSlope = 5.0
    builder.addInput("in1")
    // dots
    builder.use("in1").chainMiddle("mi11",0.6,0.5).loop("loop1",1.0,0.5,1.0).chainMiddle("mi12",1.0,0.75).chainOutput("out1",1.0)
    builder.use("loop1").setPriority(-1)
    builder.use("mi11").setForgetting(0.2)
    builder.use("out1").connect("mi11", -0.49)
    builder.use("out1").connect("mi12", -1.0)
     // lines
    builder.use("in1").chainMiddle("mi21",0.4,0.5).loop("loop2",1.0,0.5,1.0).chainMiddle("mi22",1.0,0.65).chainOutput("out2",1.0)
    builder.use("loop2").setPriority(-1)
    builder.use("mi21").setForgetting(0.05)
    builder.use("mi22").connect("mi21", -0.35)
    
    // if line then not dot
    builder.use("mi21").connect("mi12",-1.0)
    builder.use("mi21").connect("loop1",-1.0)
    
    val (in, net, out) = builder.build("in","out")
    val out1 = builder.get("out1")
    val out2 = builder.get("out2")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(out1, () => {
      println("KROPA!")
      sb.append('.'); 
    })
    out.addAfterFireTrigger(out2, () => {
      println("KRECHA!")
      sb.append('-')
    })
    
    (in, sb)
  }
  
  @Test
  def shouldDotNotLine1(){
	val (in, sb) = dotLineNet    
	in += "1,0,0,0,0,0"
	in.tickUntilCalm()
	assertEquals(".",sb.toString)
  }
  
  @Test
  def shouldDotThenLine(){
	val (in, sb) = dotLineNet    
	in += "1,0,0,1,1,0"
	in.tickUntilCalm()
	assertEquals(".-",sb.toString)
  }
    

}