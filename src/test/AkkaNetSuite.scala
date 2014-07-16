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
    val net = system.actorOf(Props(new AkkaNet("net1")),name="net1")

    val f = net ? GetId
    val msg = Await.result(f, timeout.duration).asInstanceOf[Msg]
    assertEquals("net1",msg.str)
  }
  
  @Test
  def shouldAddNeurons(){
    val net = system.actorOf(Props(new AkkaNet("net1")),name="net1")

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
  }
}