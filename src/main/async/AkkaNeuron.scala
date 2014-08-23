package main.async

import akka.actor._
import scala.collection.mutable
import main.logger.LOG
import main.utils.Utils._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._
import main.NeuronTriggers

case class AkkaSynapse(val dest: NeuronRef,val weight: Double)

case class Signal(s: Double)
case object Init
case class Msg(d: Double, str: String)
case object GetId
case object GetInput
case object GetLastOutput
case object HushNow // become silent
case class Connect(destinationRef: NeuronRef, weight: Double)
case class Disconnect(destinationId: String)
case class FindSynapse(destinationId: String)
case class MsgSynapse(synapseOpt: Option[AkkaSynapse])
case class UpdateSynapse(destinationId: String, synapse: AkkaSynapse)
case object GetSynapses
case class MsgSynapses(synapses: List[AkkaSynapse])
case object Success
case class Failure(error: String)
case object NeuronShutdown
case class NeuronShutdownDone(id: String)
case class AddAfterFireTrigger(id: String, f: (AkkaNeuron) => Any)

class AkkaNeuron(val id: String, val treshold: Double, val slope: Double, var forgetting: Double)
extends Actor with NeuronTriggers[AkkaNeuron] {
  protected val synapses = mutable.ListBuffer[AkkaSynapse]()
  
  def this(id: String) = this(id,0.5,20.0,0.0)
  def this(id: String, treshold: Double) = this(id, treshold, 20.0, 0.0)
  def that = this
  
  protected var buffer = 0.0
  protected var output = 0.0
  
  def input = buffer // only for debugging purposes
  def lastOutput = output // only for debugging purposes
  
  def silence(){
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
    synapses.foreach( _.dest ! Signal(output))
    afterFireTriggers.values.foreach( _(this) )
  }
  
  def connect(destination: AkkaNeuron, weight: Double) =
    throw new IllegalArgumentException("Use Connect(destinationRef: NeuronRef, weight: Double) request")
  
  protected def connect(destinationRef: NeuronRef, weight: Double):Unit = findSynapse(destinationRef.id) match {
    case Some(s) => sender ! Failure(s"a synapse to ${destinationRef.id} already exists")
    case None => synapses += new AkkaSynapse(destinationRef, weight); sender ! Success
  }
  
  protected def disconnect(destinationId: String):Unit = findSynapse(destinationId) match {
    case Some(s) => synapses -= s
    case None =>
  }
  protected def disconnect(destination: AkkaNeuron):Unit = disconnect(destination.id)
  
  def findSynapse(destinationId: String):Option[AkkaSynapse] = synapses.find(_.dest.id == destinationId)
  def findSynapse(destination: AkkaNeuron):Option[AkkaSynapse] = findSynapse(destination.id)

  protected def init(){
    addTresholdPassedTrigger("run", (_: AkkaNeuron) => that.run())
    sender ! Success
  }
  
  private def shutdown(){
    sender ! NeuronShutdownDone(id)
    context.stop(self)
  }
  
  def receive = { 
    case Init => init()
    case Signal(s) => this += s
    case GetId => sender ! Msg(0.0, id)
    case GetInput => sender ! Msg(input.toDouble, id)
    case GetLastOutput => sender ! Msg(lastOutput.toDouble, id)
    case HushNow => silence()
    case Connect(destinationRef, weight) => connect(destinationRef, weight)
    case Disconnect(destinationId) => disconnect(destinationId)
    case FindSynapse(destinationId) => sender ! MsgSynapse(findSynapse(destinationId)) 
    // case UpdateSynapse
    case GetSynapses => sender ! MsgSynapses(synapses.toList)
    case NeuronShutdown => shutdown()
    case AddAfterFireTrigger(id, f) => addAfterFireTrigger(id, f)
  }

}
