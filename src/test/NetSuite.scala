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
    assertEquals(0.0,out.output,0.01)
    
    net.setInput(Seq(TRESHOLD + 0.1))

    var outputRegistered = false
    out.addAfterTickTrigger("out1trigger", (n:Neuron) => {
      assertTrue(n.output > 0.01)
      outputRegistered = true
    })
    
    net.tick()
    assertTrue(outputRegistered)  
  }
  
  @Test
  def shouldSendSignalIMONet(){
    val net = IMONet(1,1,1)
    
    val out = net.find(net.outputIds(0)).get
    assertEquals(0.0,out.output,0.01)
    
    net.setInput(Seq(TRESHOLD + 0.1))

    var outputRegistered = false
    out.addAfterTickTrigger("out1trigger", (n:Neuron) => {
      assertTrue(n.output > 0.01)
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
    assertEquals(0.0,out.output,0.01)
    
    var outputRegistered = false
    out.addAfterTickTrigger("out1trigger", (n:Neuron) => {
      assertTrue(n.output > 0.01)
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
    assertEquals(0.0,out.output,0.01)
    
    net.setInput(Seq(TRESHOLD + 0.1))
    
    var outputRegistered = false
    out.addAfterTickTrigger("out1trigger", (n:Neuron) => {
      assertTrue(n.output > 0.01)
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
    out.addTrigger(outId, (n:Neuron) => {
      println( n.id + " => " + n.output )
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
    out.addTrigger(outId, (n:Neuron) => {
      println( n.id + " => " + n.output )
      outputRegistered = true
    })
    
    in.tick(3)
    
    assertTrue(outputRegistered) 
  }
  
  private def assertOutputAfter(in: NetInput, net: Net, out: NetOutput, iterations: Int) = {
    val outId = out.ids(0)
    
    @volatile var outputRegistered = false
    out.addTrigger(outId, (n:Neuron) => {
      println(s"fired!, outId=$outId, net tick=${net.iteration}")
      outputRegistered = true
    })
    
    while(!outputRegistered && net.iteration < 100) in.tick()
    
    assertTrue(outputRegistered)
    assertEquals(iterations, net.iteration)
  }
  
  @Test
  def shouldSendOutputWith2IterDelay(){
    val builder = NetBuilder()
    builder.addInput().chainMiddle(0.6,0.25).loop(0.7,0.25,1.0).chainOutput(0.8,0.75)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,0,0"
      
    assertOutputAfter(in, net, out, 2)
  }
  
  @Test
  def shouldSendOutputWith3IterDelay(){
    val builder = NetBuilder()
    builder.addInput().chainMiddle(0.55,0.25).loop(0.7,0.25,1.0).chainOutput(0.6,0.5)
    val (in, net, out) = builder.build("in1","out1")
    in += "1,0,0"
      
    assertOutputAfter(in, net, out, 3)
  }
}