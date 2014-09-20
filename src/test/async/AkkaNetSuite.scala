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

class AkkaNetSuite extends JUnitSuite {
  val system = ActorSystem("AkkaNeuronSystem")
    
  implicit val timeout = Timeout(5 seconds)
    
  @Test
  def shouldCreateNet(){
    val net = NetRef("net1")

    val f = net ? GetId
    val msg = Await.result(f, timeout.duration).asInstanceOf[Msg]
    assertEquals("net1",msg.str)
    
    net ! Shutdown
  }
  
  @Test
  def shouldAddNeurons(){
    val net = NetRef("net1")

    net ! Init
    
    val n1 = NeuronRef("id1", system)
    val n2 = NeuronRef("id2", system)

    net ! AddNeuron(n1)
    net ! AddNeuron(n2)
    Thread.sleep(500L)
    
    val f = net ? GetNeurons
    val msg = Await.result(f, timeout.duration).asInstanceOf[MsgNeurons]
    val neurons = msg.neurons
    assertEquals(2, neurons.size)
    assertTrue(neurons.map{ _.id }.contains(n1.id))
    assertTrue(neurons.map{ _.id }.contains(n2.id))
    
    net ! Shutdown
  }
  
  @Test
  def shouldConnectNeurons(){
    val net = NetRef("net1")
    
    net ! Init
    
    net ! CreateNeuron("id1")
    net ! CreateNeuron("id2")
    Thread.sleep(500L)
    
    val f1 = net ? GetNeurons
    val msg1 = Await.result(f1, timeout.duration).asInstanceOf[MsgNeurons]
    val neurons = msg1.neurons
    assertEquals(2, neurons.size)
    
    net ! ConnectNeurons("id1", "id2", 1.0)
    Thread.sleep(500L)
    
    val n1 = neurons.find( _.id == "id1").get
    val f2 = n1 ? FindSynapse("id2")
    val msg2 = Await.result(f2, timeout.duration).asInstanceOf[MsgSynapse]
    assertFalse(msg2.synapseOpt == None)
     
    net ! Shutdown
  }
  
  @Test
  def shouldSendSignal(){
    val net = NetRef("net1")
    
    net ! Init
    Thread.sleep(500L)
    
    net ! CreateNeuron("id1")
    net ! CreateNeuron("id2")
    Thread.sleep(500L)
    val cn = net ? ConnectNeurons("id1", "id2", 1.0)
    Await.result(cn, timeout.duration).asInstanceOf[Answer] match {
      case Failure(str) => fail(str)
      case Success =>
    }
    
    net ! SetInputLayer(Seq("id1"))
    net ! SetOutputLayer(Seq("id2"))
    
    val f1 = net ? GetNeuron("id2")
    val msg1 = Await.result(f1, timeout.duration).asInstanceOf[Option[NeuronRef]]
    assertFalse(msg1 == None)
    val out = msg1.get
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
    LOG.addLogToStdout("shouldBuildNet")
    val SLOPE = 20.0
    val TRESHOLD = 0.5
    val WEIGHT = 1.0
  
    val builder = AkkaNetBuilder(system)
    builder.defSlope = SLOPE
    builder.defTreshold = TRESHOLD
    builder.defWeight = WEIGHT
    builder.addInput("in1")
           .chainOutput("out1")
    val net = builder.build
    assertEquals(1, net.inputSize)
    assertEquals(1, net.outputSize)

    val out = net.find("out1").get
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
}