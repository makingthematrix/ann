package test

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import akka.actor._
import main._

import scala.concurrent._
import scala.concurrent.duration._
import akka.pattern.ask
import akka.util.Timeout

class AkkaNetSuite extends JUnitSuite {
  val system = ActorSystem("AkkaNeuronSystem")
    
  implicit val timeout = Timeout(5 seconds)
    
  @Test
  def shouldCreateNet(){
    val net = AkkaNet("net1")

    val f = net ? GetId
    val msg = Await.result(f, timeout.duration).asInstanceOf[Msg]
    assertEquals("net1",msg.str)
    
    net ! Shutdown
  }
  
  @Test
  def shouldAddNeurons(){
    val net = AkkaNet("net1")

    val n1 = AkkaRef("id1", system)
    val n2 = AkkaRef("id2", system)

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
    val net = AkkaNet("net1")
    
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
}