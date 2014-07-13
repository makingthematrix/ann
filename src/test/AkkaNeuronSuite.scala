package test

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.Await
import akka.pattern.ask
import main._
import scala.concurrent._
import ExecutionContext.Implicits.global

import akka.testkit.TestActorRef

abstract class TestActor(actorRef: ActorRef) extends Actor
case object AskForId

class AkkaNeuronSuite extends JUnitSuite {
  val SLOPE = 20.0
  val TRESHOLD = 0.5
  
  @Test
  def shouldCreateNeuron(){
    val system = ActorSystem("AkkaNeuronSystem")
    val an = system.actorOf(Props(new AkkaNeuron("n1")), name = "n")
    
    var result = 0
    
    val ta = system.actorOf(Props(new Actor {
      def receive = {
        case AskForId => an ! GetId
        case Msg(d: Double, id: String) => assertEquals("n1",id); result = 1;
      }
    }), name="t")
    
    
    ta ! AskForId
    
    Thread.sleep(1000L)
    if(result == 0) fail("Timeout")
  }
  

}