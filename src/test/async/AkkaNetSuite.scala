package test.async

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import akka.actor._
import main._
import scala.concurrent._
import scala.concurrent.duration._
import akka.util.Timeout
import main.async._

import main.logger.LOG
import main.logger.LOG._
import Messages._
import Context._
import main.utils.Utils.await

class AkkaNetSuite extends JUnitSuite {    
  @Test
  def shouldCreateNet(){
    val net = NetRef("net1")

    val msg = await[Msg](net.ref, GetId)
    assertEquals("net1",msg.str)
    
    net ! Shutdown
  }
  
  @Test
  def shouldCreateNeurons(){
    val net = NetRef("net1")

    val n1 = net.createNeuron("id1", TRESHOLD, SLOPE, FORGETTING)
    val n2 = net.createNeuron("id2", TRESHOLD, SLOPE, FORGETTING)
    net ! Init
    
    Thread.sleep(50L)
    
    val msg = await[MsgNeurons](net, GetNeurons)
    val neurons = msg.neurons
    assertEquals(2, neurons.size)
    assertTrue(neurons.map{ _.id }.contains(n1.id))
    assertTrue(neurons.map{ _.id }.contains(n2.id))
    
    net ! Shutdown
  }
  
  @Test
  def shouldConnectNeurons(){
    val net = NetRef("net1")
    
    net.createNeuron("id1", TRESHOLD, SLOPE, FORGETTING)
    net.createNeuron("id2", TRESHOLD, SLOPE, FORGETTING)
    net ! Init
    
    Thread.sleep(50L)
    
    val msg1 = await[MsgNeurons](net, GetNeurons)
    val neurons = msg1.neurons
    assertEquals(2, neurons.size)
    
    net ! ConnectNeurons("id1", "id2", 1.0)
    Thread.sleep(50L)
    
    val n1 = neurons.find( _.id == "id1").get
    val msg2 = await[MsgSynapse](n1, FindSynapse("id2"))
    assertFalse(msg2.synapseOpt == None)
     
    net ! Shutdown
  }
  
  @Test
  def shouldSendSignal(){
    LOG.addLogToStdout()
    debug("1")
    val net = NetRef("net1")
    
    debug("2")
    net.createNeuron("id1", TRESHOLD, SLOPE, FORGETTING)
    net.createNeuron("id2", TRESHOLD, SLOPE, FORGETTING)
 
    Thread.sleep(50L)
    
    debug("4")
    
    net.connectNeurons("id1", "id2", WEIGHT) match {
      case Failure(str) => fail(str)
      case Success(id) => assertEquals("net1_connectNeurons(id1,id2)",id)
    }
    debug("5")
    
    net ! SetInputLayer(Seq("id1"))
    net ! SetOutputLayer(Seq("id2"))
    
    debug("6")
    
    val msg1 = await[MsgNeuron](net, GetNeuron("id2"))
    assertFalse(msg1.neuronOpt == None)
    val out = msg1.neuronOpt.get
    assertEquals("id2",out.id)
    assertEquals(0.0,out.lastOutput,0.01) // the 'lastOutput' here is a method sending msg to out.ref and waiting for the answer
    
    net ! Init

    var outputRegistered = false
    out ! AddAfterFireTrigger("out1trigger", (n:AkkaNeuron) => {
      assertTrue(n.lastOutput > 0.01)
      outputRegistered = true
    })
    
    net ! SignalSeq(Seq(1.0)) // there is no 'tick' in AkkaNet. sending a signal triggers the calculations.
    Thread.sleep(500L)
    
    assertTrue(outputRegistered)
    
    net ! Shutdown
  }
  
  @Test
  def shouldBuildNet(){
    LOG.addLogToStdout()
  
    val builder = AkkaNetBuilder()
    builder.defSlope = SLOPE
    builder.defTreshold = TRESHOLD
    builder.defWeight = WEIGHT
    builder.addInput("in1")
           .chainOutput("out1")
    val net = builder.build
    assertEquals(1, net.inputSize)
    assertEquals(1, net.outputSize)

    val out = net.find("out1").neuronOpt.get
    assertEquals("out1",out.id)
    assertEquals(0.0,out.lastOutput,0.01) // the 'lastOutput' here is a method sending msg to out.ref and waiting for the answer
       
    net ! Init

    var outputRegistered = false
    out.addAfterFireTrigger("out1trigger", (n:AkkaNeuron) => {
      assertTrue(n.lastOutput > 0.01)
      outputRegistered = true
    })
    
    net ! SignalSeq(Seq(1.0)) // there is no 'tick' in AkkaNet. sending a signal triggers the calculations.
    Thread.sleep(500L)
    
    assertTrue(outputRegistered)
    
    net ! Shutdown
  }
  
  @Test
  def shouldBuildNetWithLoop(){
    LOG.addLogToStdout()
    
    val builder = AkkaNetBuilder()
    builder.defTreshold = 0.5
    
    debug("1")
    builder.addInput()
    debug("2")
    builder.chainMiddle()
    debug("3")
    builder.loop()
    debug("4")
    builder.chainOutput()
    debug("5")
    val net = builder.build
    debug("6")
    assertEquals(1, net.inputSize)
    assertEquals(2, net.middleSize)
    assertEquals(1, net.outputSize)
    debug("7")
    val out = net.find(net.outputIds(0)).neuronOpt.get
    debug("7.5")
    assertEquals(0.0,out.lastOutput,0.01)
    debug("8")
    net ! Init
    debug("9")
    var outputRegistered = 0
    out.addAfterFireTrigger("out1trigger", (n:AkkaNeuron) => {
      assertTrue(n.lastOutput > 0.01)
      if(outputRegistered < 2) outputRegistered += 1
    })
    debug("10")
    net ! SignalSeq(Seq(1.0)) // there is no 'tick' in AkkaNet. sending a signal triggers the calculations.
    Thread.sleep(500L)
    assertEquals(2, outputRegistered)
    debug("11")
    net ! Shutdown
    debug("12")
  }

  @Test
  def shouldUseInputAndOutput1(){
    LOG.addLogToStdout()
    debug("1")
    val builder = AkkaNetBuilder()
    debug("2")
    builder.defTreshold = TRESHOLD
    builder.addInput().chainMiddle().loop().chainOutput()
    debug("3")
    val net = builder.build
    debug("4")
    val in = AkkaNetInput("in1", net)
    debug("5")
    in += TRESHOLD + 0.1
    debug("6")
    val out = AkkaNetOutput("out1", net)
    debug("7")
    
    net ! Init
    Thread.sleep(50L)
    debug("8")
    
    var outputRegistered = false
    out.addAfterFireTrigger(out.getId(0), (n:AkkaNeuron) => {
      println( n.id + " => " + n.lastOutput )
      outputRegistered = true
    })
    
    in.tick(3)
    debug("9")
    Thread.sleep(500L)
    assertTrue(outputRegistered)
    debug("10")
    
    net ! Shutdown
    debug("11")
  }
  
  @Test
  def shouldUseInputAndOutput2(){
    LOG.addLogToStdout()
    
    val builder = AkkaNetBuilder()
    builder.defTreshold = TRESHOLD
    builder.addInput().chainMiddle().loop().chainOutput()
    
    val net = builder.build
    
    val in = AkkaNetInput("in1", net)
    in += TRESHOLD + 0.1
    in += 0.0
    in += 0.0
    in += TRESHOLD + 0.1
    in += 0.0
    in += 0.0
    in += TRESHOLD + 0.1
    
    val out = AkkaNetOutput("out1", net)
    val outId = out.getId(0)
    
    var outputRegistered = false
    out.addAfterFireTrigger(outId, (n:AkkaNeuron) => {
      debug( n.id + " => " + n.lastOutput )
      outputRegistered = true
    })

    net ! Init
    Thread.sleep(50L)
    
    in.tick(3)
    Thread.sleep(500L)
    assertTrue(outputRegistered)
    
    net ! Shutdown
  }
}