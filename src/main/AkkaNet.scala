package main

import akka.actor._
import scala.collection.mutable
import scala.concurrent.Await
import akka.util.Timeout
import scala.concurrent.duration._

case object GetNeurons
case class MsgNeurons(neurons: List[AkkaRef])
case class AddNeuron(nref: AkkaRef)
case class CreateNeuron(id: String)
case class ConnectNeurons(id1: String, id2: String, weight: Double)
case object Shutdown

class AkkaNet(val id: String) extends Actor with AbstractNet[AkkaRef] {
  implicit val timeout = Timeout(5 seconds)
  
  private val neurons = mutable.ListBuffer[AkkaRef]()
  private val ins = mutable.ListBuffer[AkkaRef]()
  private val outs = mutable.ListBuffer[AkkaRef]()
  
  override protected def inputLayer = ins.toSeq
  override protected def outputLayer = outs.toSeq
  override protected def middleLayer = {
    val inputIds = ins.map( _.id ).toSet
    val outputIds = outs.map( _.id ).toSet
    neurons.filterNot( n => inputIds.contains(n.id) || outputIds.contains(n.id))
  }
  
  private val awaitingAnswers = mutable.Map[ActorRef,ActorRef]()
  
  private def answer(ref: ActorRef, msg: Any) = awaitingAnswers.remove(ref) match {
    case Some(otherRef) => otherRef ! msg
    case None => 
  }
  
  private def shutdown(){
    neurons.foreach( _.ref ! PoisonPill )
    context.stop(self)
  }
  
  def receive = {
    case GetId => sender ! Msg(0.0, id)
    case GetNeurons => sender ! MsgNeurons(neurons.toList)
    case AddNeuron(nref) => neurons += nref
    case CreateNeuron(id) => neurons += AkkaRef(id)
    case ConnectNeurons(id1, id2, weight) => connectNeurons(id1, id2, weight)
    case Success => answer(sender, Success)
    case failure: Failure => answer(sender, failure) 
    case Shutdown => shutdown()
  }
  
  private def connectNeurons(id1: String, id2: String, weight: Double) = findRef(id1, id2) match {
    case (Some(ref1),Some(ref2)) => {
      awaitingAnswers += ref1.ref -> sender
      ref1 ! Connect(ref2, weight)
    }
    case (Some(ref1),None) => sender ! Failure(s"There is no neuron with id $id2")
    case (None,Some(ref2)) => sender ! Failure(s"There is no neuron with id $id1")
    case (None, None) => sender ! Failure(s"There is neither neuron with id $id1 nor $id2")
  }
  
  private def findRef(id: String):Option[AkkaRef] = neurons.find(_.id == id)
  private def findRef(id1: String, id2: String):(Option[AkkaRef],Option[AkkaRef]) = (findRef(id1), findRef(id2))
}

object AkkaNet{
  val system = ActorSystem("AkkaNeuronSystem")
  
  def apply(id: String):ActorRef = apply(id, system)
  
  def apply(id: String, system: ActorSystem):ActorRef = system.actorOf(Props(new AkkaNet(id)))
}