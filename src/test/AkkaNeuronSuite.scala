package test

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import akka.actor._
import main._

case object AskForId
case class OrderConnect(n1Ref: ActorRef, n2Id: String, n2Ref: ActorRef, weight: Double)
case class AskForSynapse(n1Ref: ActorRef, n2Id: String)
case class SendSignal(s: Double)
case object OrderInit
case class AskLastOutput(ref: ActorRef)
case class AskForInput(ref: ActorRef)

class AkkaNeuronSuite extends JUnitSuite {
  val system = ActorSystem("AkkaNeuronSystem")
  val id1 = "n1"
  val n1 = system.actorOf(Props(new AkkaNeuron(id1)))
      
  @Test
  def shouldCreateNeuron(){
    var foundId = ""
    
    val ta = system.actorOf(Props(new Actor {
      def receive = {
        case AskForId => n1 ! GetId
        case Msg(_, id) => foundId = id
      }
    }))
    
    ta ! AskForId
    
    Thread.sleep(1000L)
    assertEquals(id1, foundId)
  }
  
  @Test
  def shouldConnect(){
    val id2 = "n2"
    val n2 = system.actorOf(Props(new AkkaNeuron(id2)))
    
    var success = false 
    var synapseOption:Option[AkkaSynapse] = None
    
    val ta = system.actorOf(Props(new Actor {
      def receive = {
        case Success => success = true
        case OrderConnect(n1Ref, n2Id, n2Ref, weight) => n1Ref ! Connect(n2Id, n2Ref, weight)
        case AskForSynapse(n1Ref, n2Id) => n1Ref ! FindSynapse(n2Id)
        case MsgSynapse(so) => synapseOption = so
      }
    }))
    
    ta ! OrderConnect(n1, id2, n2, 1.0)
    Thread.sleep(1000L)
    assertTrue(success)
    
    ta ! AskForSynapse(n1, id2)
    Thread.sleep(1000L)
    assertTrue(synapseOption != None)
    val s = synapseOption.get
    assertEquals(id2, s.destinationId)
  }
  
  @Test
  def shouldGetOutput(){
    var success = false
    var lastOutput = -1.0
    
    val ta = system.actorOf(Props(new Actor {
      def receive = {
        case OrderInit => n1 ! Init
        case Success => success = true
        case SendSignal(s) => n1 ! Signal(s)
        case AskLastOutput(n) => n ! GetLastOutput
        case Msg(output, _) => lastOutput = output
      }
    }))
    
    ta ! OrderInit
    Thread.sleep(500L)
    assertTrue(success)
    
    ta ! SendSignal(1.0)
    Thread.sleep(500L)
    
    ta ! AskLastOutput(n1)
    Thread.sleep(500L)
    
    assertTrue(lastOutput > 0.0)
  }
  
  @Test
  def shouldSendSignal(){
    var success = false
    val id2 = "n2"
    val n2 = system.actorOf(Props(new AkkaNeuron(id2)))
    var signal = -1.0
    
    val ta = system.actorOf(Props(new Actor {
      def receive = {
        case OrderInit => n1 ! Init
        case Success => success = true
        case SendSignal(s) => n1 ! Signal(s)
        case AskLastOutput(n) => n ! GetLastOutput
        case Msg(output, _) => signal = output
        case OrderConnect(n1Ref, n2Id, n2Ref, weight) => n1Ref ! Connect(n2Id, n2Ref, weight)
        case AskForInput(n) => n ! GetInput
      }
    }))
    
    ta ! OrderConnect(n1, id2, n2, 1.0)
    Thread.sleep(500L)
    assertTrue(success)
    success = false
    
    ta ! OrderInit
    Thread.sleep(500L)
    assertTrue(success)
    
    ta ! AskForInput(n1)
    Thread.sleep(500L)
    assertEquals(0.0, signal, 0.01)
    signal = -1.0
    
    ta ! SendSignal(1.0)
    Thread.sleep(500L)
    
    ta ! AskForInput(n2)
    Thread.sleep(500L)
    assertTrue(signal > 0.0)
  }
  
    @Test
  def shouldRespectTreshold(){
    val TRESHOLD = 0.5
    var success = false
    val id2 = "n2"
    val n2 = system.actorOf(Props(new AkkaNeuron(id2, TRESHOLD)))
    var signal = -1.0
  
    
    val ta = system.actorOf(Props(new Actor {
      def receive = {
        case OrderInit => n1 ! Init
        case Success => success = true
        case SendSignal(s) => n1 ! Signal(s)
        case AskLastOutput(n) => n ! GetLastOutput
        case Msg(output, _) => signal = output
        case OrderConnect(n1Ref, n2Id, n2Ref, weight) => n1Ref ! Connect(n2Id, n2Ref, weight)
        case AskForInput(n) => n ! GetInput
      }
    }))
    
    ta ! OrderConnect(n1, id2, n2, 1.0)
    Thread.sleep(500L)
    assertTrue(success)
    success = false
    
    ta ! OrderInit
    Thread.sleep(500L)
    assertTrue(success)
    
    ta ! AskForInput(n1)
    Thread.sleep(500L)
    assertEquals(0.0, signal, 0.01)
    signal = -1.0
    
    ta ! SendSignal(TRESHOLD - 0.1)
    Thread.sleep(500L)
    
    ta ! AskForInput(n2)
    Thread.sleep(500L)
    assertEquals(0.0, signal, 0.01)
  }
}