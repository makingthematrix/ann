package main

import akka.actor._
import scala.collection.mutable
import main.logger.LOG
import main.utils.Utils._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._

case class Signal(s: Double)
case object Init
case class Msg(d: Double, str: String)
case object GetId
case object GetInput
case object GetLastOutput
case object HushNow // become silent
case class Connect(destinationId: String, destinationRef: ActorRef, weight: Double)
case class Disconnect(destinationId: String)
case class FindSynapse(destinationId: String)
case class MsgSynapse(synapseOpt: Option[AkkaSynapse])
case class UpdateSynapse(destinationId: String, synapse: AkkaSynapse)
case object GetSynapses
case class MsgSynapses(synapses: List[AkkaSynapse])
case object Success
case class Failure(error: String)

class AkkaNeuron(val id: String, val treshold: Double, val slope: Double, var forgetting: Double, var priority: Int)
extends Actor with NeuronLike with NeuronTriggers[AkkaNeuron] {
  def this(id: String) = this(id,0.5,20.0,0.0,0)
  def this(id: String, treshold: Double) = this(id, treshold, 20.0, 0.0, 0)
  def that = this
  
  protected var buffer = 0.0
  protected var output = 0.0
  
  override def getId = id
  
  override def input = buffer // only for debugging purposes
  override def lastOutput = output // only for debugging purposes
  
  override def silence(){
    buffer = 0.0
    output = 0.0
  }
  
  protected def calculateOutput = minMaxOpen(buffer, 0.0, 1.0, 1.0/(1.0+Math.exp(-slope*(buffer-0.5))) )
    // = 2/(1+EXP(-C*x))-1 ; mapowanie S -1->-1,0->0,1->1, gdzie C to stromość
    // = 1/(1+EXP(-C*(x-0.5))) ; mapowanie S 0->0,0.5->0.5,1->1, gdzie C to stromość
  
  protected def tickForgetting() = 
    if(buffer > 0.0) buffer = Math.max(buffer - forgetting, 0.0)
    else if(buffer < 0.0) buffer = Math.min(buffer + forgetting, 0.0)
     // might be changed into the S function later on
  
  def +=(signal: Double) = {
    buffer = minmax(-1.0, buffer+signal, 1.0)
    if(buffer > treshold) tresholdPassedTriggers.values.foreach( _(this) )
  }
  
  protected def run() = {
    output = calculateOutput
    buffer = 0.0
    //println(s"output $output")
    synapses foreach { _.destinationRef ! Signal(output) } 
    afterFireTriggers.values.foreach( _(this) )
  }
  
  def connect(destinationId:String, destinationRef: ActorRef, weight: Double):Unit = findSynapse(destinationId) match {
    case Some(s) => sender ! Failure(s"a synapse to $destinationId already exists")
    case None => synapses += new AkkaSynapse(destinationId, destinationRef, weight); sender ! Success
  }
  def connect(destination: NeuronLike, weight: Double):Unit = 
    throw new IllegalArgumentException("Use connect(String, ActorRef, Double)")
  
  def disconnect(destinationId: String):Unit = findSynapse(destinationId) match {
    case Some(s) => synapses -= s
    case None =>
  }
  def disconnect(destination: NeuronLike):Unit = disconnect(destination.getId)
  
  def findSynapse(destinationId: String):Option[AkkaSynapse] = synapses.find(_.destinationId == destinationId)
  def findSynapse(destination: NeuronLike):Option[AkkaSynapse] = findSynapse(destination.getId)
  
  override def getSynapses = synapses.toList
  
  protected val synapses = mutable.ListBuffer[AkkaSynapse]()
  
  protected def init(){
    addTresholdPassedTrigger("run", (_: NeuronLike) => that.run())
    sender ! Success
  }
  
  def receive = { 
    case Init => init()
    case Signal(s) => this += s
    case GetId => sender ! Msg(0.0, id)
    case GetInput => sender ! Msg(input.toDouble, id)
    case GetLastOutput => sender ! Msg(lastOutput.toDouble, id)
    case HushNow => silence()
    case Connect(destinationId, destinationRef, weight) => connect(destinationId, destinationRef, weight)
    case Disconnect(destinationId) => disconnect(destinationId)
    case FindSynapse(destinationId) => sender ! MsgSynapse(findSynapse(destinationId)) 
    // case UpdateSynapse
    case GetSynapses => sender ! MsgSynapses(getSynapses)
  }
}

class AkkaRef(val id: String, val ref: ActorRef) extends NeuronLike {
  implicit var timeout = Timeout(5 seconds)
  
  override def getId = id
  
  override def input = Await.result(ref ? GetInput, timeout.duration).asInstanceOf[Msg].d
  override def lastOutput = Await.result(ref ? GetLastOutput, timeout.duration).asInstanceOf[Msg].d
  override def silence() = ref ! HushNow
  
  override protected def calculateOutput = Double.NaN // we don't do that here 
  override def getSynapses: List[Synapse] = Await.result(ref ? GetSynapses, timeout.duration).asInstanceOf[MsgSynapses].synapses
  
  def +=(signal: Double) = ref ! Signal(signal) 
  
  def !(any: Any) = ref ! any
  def ?(any: Any) = ref ? any
} 

object AkkaRef {
  val system = ActorSystem("AkkaNeuronSystem")
  
  def apply(id: String):AkkaRef = apply(id, system)
  
  def apply(id: String, system: ActorSystem):AkkaRef = {
    val ref = system.actorOf(Props(new AkkaNeuron(id)), name=id)
    new AkkaRef(id, ref)
  }
}