package test

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import main._

class NetSuite extends JUnitSuite {
  val SLOPE = 20.0
  val HARD_TRESHOLD = 0.5
  val WEIGHT = 1.0
  
  @Test
  def shouldCreateNet(){
    val net = Net(SLOPE, HARD_TRESHOLD, WEIGHT, 7)
    assertEquals(7, net.size)
  }
  
  @Test
  def shouldAddNeurons(){
    val net = Net()
    val n1 = net.addNeuron(SLOPE, HARD_TRESHOLD)
    val n2 = net.addNeuron(SLOPE, HARD_TRESHOLD)
    assertEquals(2, net.size)
    val idSet = net.ids.toSet
    assertTrue(idSet.contains(n1.id))
    assertTrue(idSet.contains(n2.id))
  }
  
  @Test
  def shouldConnectNeurons(){
    val net = Net()
    val n1 = net.addNeuron(SLOPE, HARD_TRESHOLD)
    val n2 = net.addNeuron(SLOPE, HARD_TRESHOLD)
    net.connect(n1.id, n2.id, 1.0)
    val s = n1.findSynapse(n2)
    assertFalse(s == None)
  }
  
  @Test
  def shouldSendSignal(){
    val net = Net(SLOPE, HARD_TRESHOLD, WEIGHT)
    val n1 = net.addNeuron()
    val n2 = net.addNeuron()
    net.connect(n1, n2)
    net.setInputLayer(Seq(n1.id))
    assertEquals(1, net.inputSize)
    net.setOutputLayer(Seq(n2.id))
    assertEquals(1, net.outputSize)
    net.setInput(Seq(HARD_TRESHOLD + 0.1))
    
    val output1 = net.output
    assertEquals(1,output1.size)
    assertEquals(0.0, output1(0), 0.01)
    
    net.tick()
    
    val output2 = net.output
    assertTrue(output2(0) > 0.01)  
  }
  
  @Test
  def shouldSendSignalIMONet(){
    val net = IMONet(1,1,1)
    net.setInput(Seq(HARD_TRESHOLD + 0.1))
    
    val output1 = net.output
    assertEquals(1,output1.size)
    assertEquals(0.0, output1(0), 0.01)
    
    net.tick()
    
    val output2 = net.output
    assertTrue(output2(0) > 0.01)
  }
  
  @Test
  def shouldBuildNet(){
    val builder = NetBuilder()
    builder.slope = SLOPE
    builder.hardTreshold = HARD_TRESHOLD
    builder.defWeight = WEIGHT
    builder.addInput("in1").chainMiddle("m1").chainOutput("out1")
    val net = builder.build
    assertEquals(1, net.inputSize)
    assertEquals(1, net.outputSize)
       
    net.setInput(Seq(HARD_TRESHOLD + 0.1))
    
    val output1 = net.output
    assertEquals(1,output1.size)
    assertEquals(0.0, output1(0), 0.01)
    
    net.tick()
    
    val output2 = net.output
    assertTrue(output2(0) > 0.01)  
  }
  
  @Test
  def shouldBuildNetWithLoop(){
    val builder = NetBuilder()
    builder.slope = SLOPE
    builder.hardTreshold = HARD_TRESHOLD
    builder.defWeight = WEIGHT
    builder.addInput().chainMiddle().loop().chainOutput()
    val net = builder.build
    assertEquals(1, net.inputSize)
    assertEquals(2, net.middleSize)
    assertEquals(1, net.outputSize)
       
    net.setInput(Seq(HARD_TRESHOLD + 0.1))
    
    val output1 = net.output
    assertEquals(1,output1.size)
    assertEquals(0.0, output1(0), 0.01)
    
    net.tick()
    
    val output2 = net.output
    assertTrue(output2(0) > 0.01)  
    
    net.tick()
    
    val output3 = net.output
    assertTrue(output3(0) > 0.01)  

  }
}