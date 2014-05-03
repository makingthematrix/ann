package test

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import main._

class NetSuite extends JUnitSuite {
  val SLOPE = 20.0
  val TRESHOLD = 0.5
  val WEIGHT = 1.0
  
  @Test
  def shouldCreateNet(){
    val net = Net(SLOPE, TRESHOLD, WEIGHT, 7)
    assertEquals(7, net.size)
  }
  
  @Test
  def shouldAddNeurons(){
    val net = Net()
    val n1 = net.addNeuron(SLOPE, TRESHOLD)
    val n2 = net.addNeuron(SLOPE, TRESHOLD)
    assertEquals(2, net.size)
    val idSet = net.ids.toSet
    assertTrue(idSet.contains(n1.id))
    assertTrue(idSet.contains(n2.id))
  }
  
  @Test
  def shouldConnectNeurons(){
    val net = Net()
    val n1 = net.addNeuron(SLOPE, TRESHOLD)
    val n2 = net.addNeuron(SLOPE, TRESHOLD)
    net.connect(n1.id, n2.id, 1.0)
    val s = n1.findSynapse(n2)
    assertFalse(s == None)
  }
  
  @Test
  def shouldSendSignal(){
    val net = Net(SLOPE, TRESHOLD, WEIGHT)
    val n1 = net.addNeuron()
    val n2 = net.addNeuron()
    net.connect(n1, n2)
    net.setInputLayer(Seq(n1.id))
    assertEquals(1, net.inputSize)
    net.setOutputLayer(Seq(n2.id))
    assertEquals(1, net.outputSize)
    
    val out = net.find(net.outputIds(0)).get
    assertEquals(0.0,out.lastOutput,0.01)
    
    net.setInput(Seq(TRESHOLD + 0.1))

    var outputRegistered = false
    out.addAfterFireTrigger("out1trigger", (n:Neuron) => {
      assertTrue(n.lastOutput > 0.01)
      outputRegistered = true
    })
    
    net.tick()
    assertTrue(outputRegistered)  
  }
  
  @Test
  def shouldSendSignalIMONet(){
    val net = IMONet(1,1,1)
    
    val out = net.find(net.outputIds(0)).get
    assertEquals(0.0,out.lastOutput,0.01)
    
    net.setInput(Seq(TRESHOLD + 0.1))

    var outputRegistered = false
    out.addAfterFireTrigger("out1trigger", (n:Neuron) => {
      assertTrue(n.lastOutput > 0.01)
      outputRegistered = true
    })
    
    net.tick()
    assertTrue(outputRegistered)
  }
  
  @Test
  def shouldBuildNet(){
    val builder = NetBuilder()
    builder.defSlope = SLOPE
    builder.defTreshold = TRESHOLD
    builder.defWeight = WEIGHT
    builder.addInput("in1").chainMiddle("m1").chainOutput("out1")
    val net = builder.build
    assertEquals(1, net.inputSize)
    assertEquals(1, net.outputSize)
       
    net.setInput(Seq(TRESHOLD + 0.1))
    
    val out = net.find(net.outputIds(0)).get
    assertEquals(0.0,out.lastOutput,0.01)
    
    var outputRegistered = false
    out.addAfterFireTrigger("out1trigger", (n:Neuron) => {
      assertTrue(n.lastOutput > 0.01)
      outputRegistered = true
    })

    net.tick()
    assertTrue(outputRegistered)  
  }
  
  @Test
  def shouldBuildNetWithLoop(){
    val builder = NetBuilder()
    builder.defSlope = SLOPE
    builder.defTreshold = TRESHOLD
    builder.defWeight = WEIGHT
    builder.addInput().chainMiddle().loop().chainOutput()
    val net = builder.build
    assertEquals(1, net.inputSize)
    assertEquals(2, net.middleSize)
    assertEquals(1, net.outputSize)
    
    val out = net.find(net.outputIds(0)).get
    assertEquals(0.0,out.lastOutput,0.01)
    
    net.setInput(Seq(TRESHOLD + 0.1))
    
    var outputRegistered = false
    out.addAfterFireTrigger("out1trigger", (n:Neuron) => {
      assertTrue(n.lastOutput > 0.01)
      outputRegistered = true
    })
    
    net.tick()    
    assertTrue(outputRegistered)
    outputRegistered = false
    net.tick()
    assertTrue(outputRegistered)
  }
  
  @Test
  def shouldUseInputAndOutput(){
    val builder = NetBuilder()
    builder.addInput().chainMiddle().loop().chainOutput()
    
    val net = builder.build
    
    val in = NetInput("in1", net)
    in += TRESHOLD + 0.1
    in += 0.0
    in += 0.0
    in += TRESHOLD + 0.1
    in += 0.0
    in += 0.0
    in += TRESHOLD + 0.1
    
    val out = NetOutput("out1", net)
    val outId = out.ids(0)
    
    var outputRegistered = false
    out.addAfterFireTrigger(outId, (n:Neuron) => {
      println( n.id + " => " + n.lastOutput )
      outputRegistered = true
    })
    
    in.tick(3)
    
    assertTrue(outputRegistered)
  }
  
  @Test
  def shouldUserNetInputAbbreviations(){
    val builder = NetBuilder()
    builder.addInput().chainMiddle().loop().chainOutput()
    
    val (in, net, out) = builder.build("in1","out1")
    
    in.regSign('a',TRESHOLD + 0.1)
    // 0 and 1 should be registered already as "0" and "1" respectively
    in += "a,0,0,a,0,0,a"

    val outId = out.ids(0)
    
    var outputRegistered = false
    out.addAfterFireTrigger(outId, (n:Neuron) => {
      println( n.id + " => " + n.lastOutput )
      outputRegistered = true
    })
    
    in.tick(3)
    
    assertTrue(outputRegistered) 
  }
  
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
  def shouldSendOutputWith2IterDelay_usingInputSynapse(){
    val builder = NetBuilder()
    builder.addInput().chainMiddle(0.55,0.5).loop(1.0,0.5,1.0).chainOutput(1.0,0.75)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,0,0"
      
    assertOutputAfter(in, net, out, 2)
  }
  
  @Test
  def shouldSendOutputWith2IterDelay_usingSlopeAndSelf(){
    val builder = NetBuilder()
    builder.addInput().chainMiddle(0.7,0.5,5.0).self(1.0).chainOutput(1.0,0.75)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,0,0"
      
    assertOutputAfter(in, net, out, 2)
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
  def shouldCreateOscillator(){
    val builder = NetBuilder()
    builder.addInput("in1").chainMiddle("mi1",1.0).loop("osc",1.0,0.5,-1.0).chainOutput("out1",1.0,0.75)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,1,1,1,1,1"
      
    val outId = out.ids(0)
    val sb = StringBuilder.newBuilder
    var outputRegistered = false
    out.addAfterFireTrigger(outId, (n:Neuron) => {
      sb.append('1')
      println("SIGNAL!")
      outputRegistered = true
    })
    out.addAfterTickTrigger(outId, (n:Neuron) => {
      if(!outputRegistered){ 
        sb.append('0')
        println("NO SIGNAL...")
      }
      outputRegistered = false
    })
    
    in.tick(6)
    
    val str = sb.toString
    println(str)
    assertEquals("101010",str)
  }
  
  @Test
  def shouldCreateOscillatorWithMethod1(){
    val builder = NetBuilder()
    builder.addInput().chainMiddle(1.0).oscillator().chainOutput(1.0,0.75)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,1,1,1,1,1"
      
    val outId = out.ids(0)
    val sb = StringBuilder.newBuilder
    var outputRegistered = false
    out.addAfterFireTrigger(outId, (n:Neuron) => {
      sb.append('1')
      outputRegistered = true
    })
    out.addAfterTickTrigger(outId, (n:Neuron) => {
      if(!outputRegistered) sb.append('0')
      outputRegistered = false
    })
    
    in.tick(6)
    
    val str = sb.toString
    println(str)
    assertEquals("101010",str)
  }
  
  @Test
  def shouldCreateOscillatorWithMethod2(){
    val builder = NetBuilder()
    builder.addInput().chainOscillator(1.0).chainOutput(1.0,0.75)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,1,1,1,1,1"
      
    val outId = out.ids(0)
    val sb = StringBuilder.newBuilder
    var outputRegistered = false
    out.addAfterFireTrigger(outId, (n:Neuron) => {
      sb.append('1')
      outputRegistered = true
    })
    out.addAfterTickTrigger(outId, (n:Neuron) => {
      if(!outputRegistered) sb.append('0')
      outputRegistered = false
    })
    
    in.tick(6)
    
    val str = sb.toString
    println(str)
    assertEquals("101010",str)
  }
  
  @Test
  def shouldCreateOscillator2(){
    val builder = NetBuilder()
    builder.addInput("in1").chainMiddle("mi1",1.0).loop("osc",1.0,0.5,-1.0).chainOutput("out1",1.0,0.75)
    builder.use("in1").chainMiddle("mi2",1.0).chainOutput("out2",1.0,0.75)
    builder.use("osc").connect("mi2",-1.0)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,1,1,1,1,1"
      
    val out1 = builder.get("out1")
    val out2 = builder.get("out2")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(out1, (n:Neuron) => {
      sb.append('1'); 
    })
    out.addAfterFireTrigger(out2, (n:Neuron) => {
      sb.append('0')
    })
    
    in.tick(6)
    
    val str = sb.toString
    println(str)
    assertEquals("101010",str)
  }
  
    
  private def dotNet() = { // ;)
    val builder = NetBuilder()
    // dots
    builder.addInput("in1").chainMiddle("mi1",0.501,0.5).loop("loop",1.0,0.5,1.0).chainMiddle("mi2",1.0,0.75).chainOutput("out1",1.0,0.75)
    builder.use("mi2").connect("mi1", -1.0)
    builder.use("mi2").connect("loop", -1.0)
    
    val (in, net, out) = builder.build("in","out")
    val out1 = builder.get("out1")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(out1, (n:Neuron) => {
      println("KROPA!")
      sb.append('.'); 
    })
    (in, sb)    
  }
  
  @Test
  def shouldDotThenNothing1(){
    val (in, sb) = dotNet
    
    in += "1,0,0,0,0,0"
    in.tick(6)
    assertEquals(".",sb.toString)
  }
  
  @Test
  def shouldDotThenNothing2(){
    val (in, sb) = dotNet
    
    in += "0,0,1,0,0,0"
    in.tick(6)
    assertEquals(".",sb.toString)
  }
  
  @Test
  def shouldDotThenNothing3(){
    val (in, sb) = dotNet
    
    in += "0,0,1"
    in.tick(3)
    assertEquals("",sb.toString)
  }  
   
  private def lineNet() = {
    val builder = NetBuilder()
    // lines
    builder.addInput("in1").chainMiddle("mi1",0.4,0.65,5.0).loop("loop",1.0,0.5,1.0).chainMiddle("mi2",1.0,0.9).chainOutput("out1",1.0)
    builder.use("mi1").setForgetting(0.1)
    builder.use("mi2").connect("mi1", -1.0)
    builder.use("mi2").connect("loop", -1.0)
    
    val (in, net, out) = builder.build("in","out")
    val out1 = builder.get("out1")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(out1, (n:Neuron) => {
      println("KRECHA!")
      sb.append('-'); 
    })
    
    (in, sb)
  }

  @Test
  def shouldLineThenNothing1(){
    val (in, sb) = lineNet
    
    in += "1,1,0,0,0,0"
    in.tick(6)
    assertEquals("-",sb.toString)
  }
  
  @Test
  def shouldLineThenNothing2(){
    val (in, sb) = lineNet
    
    in += "0,0,1,1,0,0"
    in.tick(6)
    assertEquals("-",sb.toString)
  }
    
  @Test
  def shouldLineThenNothing3(){
    val (in, sb) = lineNet
    
    in += "0,1,0,0"
    in.tick(4)
    assertEquals("",sb.toString)
  }
  
  @Test
  def shouldLineThenNothing4(){
    val (in, sb) = lineNet
    
    in += "1,0,0,1,0,0"
    in.tick(6)
    assertEquals("",sb.toString)
  }  
 
}