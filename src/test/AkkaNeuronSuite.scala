package test

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import akka.actor._
import main._

case object AskForId
case class SendConnect(n1Ref: ActorRef, n2Id: String, n2Ref: ActorRef, weight: Double)
case class SendFindSynapse(n1Ref: ActorRef, n2Id: String)
case class SendSignal(s: Double)
case object SendInit
case object SendGetLastOutput

class AkkaNeuronSuite extends JUnitSuite {
  val id1 = "n1"
  val system = ActorSystem("AkkaNeuronSystem")
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
        case SendConnect(n1Ref, n2Id, n2Ref, weight) => n1Ref ! Connect(n2Id, n2Ref, weight)
        case Success => success = true
        case SendFindSynapse(n1Ref, n2Id) => n1Ref ! FindSynapse(n2Id)
        case MsgSynapse(so) => synapseOption = so
      }
    }))
    
    ta ! SendConnect(n1, id2, n2, 1.0)
    Thread.sleep(1000L)
    assertTrue(success)
    
    ta ! SendFindSynapse(n1, id2)
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
        case SendInit => n1 ! Init
        case Success => success = true
        case SendSignal(s) => n1 ! Signal(s)
        case SendGetLastOutput => n1 ! GetLastOutput
        case Msg(output, _) => lastOutput = output
      }
    }))
    
    ta ! SendInit
    Thread.sleep(500L)
    assertTrue(success)
    
    ta ! SendSignal(1.0)
    Thread.sleep(500L)
    
    ta ! SendGetLastOutput
    Thread.sleep(500L)
    
    assertTrue(lastOutput > 0.0)
  }
}