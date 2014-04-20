package test

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import main._

class SOSTestSuite extends JUnitSuite {
  private def assertOutputAfter(in: NetInput, net: Net, out: NetOutput, iterations: Int) = {
    val outId = out.ids(0)
    
    var outputRegistered = false
    out.addAfterFireTrigger(outId, (n:Neuron) => {
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
    builder.addInput().chainMiddle(0.501,0.5).loop(1.0,0.5,1.0).chainOutput(1.0,0.75)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,0,0"
      
    assertOutputAfter(in, net, out, 3)
  }
    
  @Test
  def shouldSendOutputWith3IterDelay_usingSlopeAndSelf(){
    val builder = NetBuilder()
    builder.addInput().chainMiddle(0.55,0.5,8.0).self(1.0).chainOutput(1.0,0.75)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,0,0"
      
    assertOutputAfter(in, net, out, 3)
  }
  
  @Test
  def shouldSendOutputWith2Signals_usingTreshold(){
    val builder = NetBuilder()
    builder.addInput().chainMiddle(0.4,0.75,5.0).loop(1.0,0.5,1.0).chainOutput(1.0,0.9)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,1,0"
      
    assertOutputAfter(in, net, out, 3)
  }
  
  @Test
  def shouldRecognizeDotsAndLines(){
    val builder = NetBuilder()
    // dots
    //builder.addInput("in1").chainMiddle("mi1",0.55,0.5,8.0).self(1.0).chainOutput("out1",1.0,0.75)
    builder.addInput("in1").chainMiddle("mi1",0.501,0.5).loop("osc1",1.0,0.5,1.0).chainOutput("out1",1.0,0.75)
    // lines
    builder.use("in1").chainMiddle("mi2",0.4,0.75,5.0).loop("osc2",1.0,0.5,1.0).chainOutput("out2",1.0,0.9)
    // if line then not dot
    builder.use("osc2").connect("osc1",-1.0)
    builder.use("osc2").connect("mi1",-1.0)
    builder.use("osc2").connect("out1",-1.0)
    
    val (in, net, out) = builder.build("in","out")
    val out1 = builder.findByName("out1")
    val out2 = builder.findByName("out2")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(out1, (n:Neuron) => {
      println("KROPA!")
      sb.append('.'); 
    })
    out.addAfterFireTrigger(out2, (n:Neuron) => {
      println("KRECHA!")
      sb.append('-')
    })
    
    //in += "1,0,0,0,0,0"
    //in.tick(3)
    //assertEquals(".",sb.toString)
    //sb.setLength(0)
    
    in += "1,1,0,0,0,0"
    in.tick(6)
    assertEquals("--",sb.toString)
    sb.clear
  }
}