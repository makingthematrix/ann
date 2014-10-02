package main.async

import akka.actor._
import scala.collection.mutable
import main.logger.LOG
import main.logger.LOG._
import main.utils.Utils._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._
import main.NeuronTriggers
import Messages._

import ExecutionContext.Implicits.global

case class Synapse(val dest: NeuronRef,val weight: Double)

class Neuron(val id: String, val treshold: Double, val slope: Double, var forgetting: Double)
extends Actor with NeuronTriggers[Neuron] {
  protected val synapses = mutable.ListBuffer[Synapse]()
  
  def this(id: String) = this(id,0.5,20.0,0.0)
  def this(id: String, treshold: Double) = this(id, treshold, 20.0, 0.0)
  def that = Neuron.this
  
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
    debug(this, s"$id adding signal $signal to buffer $buffer, treshold is $treshold")
    buffer = minmax(-1.0, buffer+signal, 1.0)
    if(buffer > treshold) tresholdPassedTriggers.values.foreach( _(this) )
  }
  
  protected def run() = {
    debug(this, s"run $id")
    output = calculateOutput
    buffer = 0.0
    debug(this, s"output $output, synapses size: ${synapses.size}")
    synapses.foreach { s => debug(this, s.dest.id) }
    synapses.foreach( _.dest ! Signal(output))
    afterFireTriggers.values.foreach( _(Neuron.this) )
  }
  
  def connect(destination: Neuron, weight: Double) =
    throw new IllegalArgumentException("Use Connect(destinationRef: NeuronRef, weight: Double) request")
  
  private def _connect(destinationRef: NeuronRef, weight: Double) = {
    debug(Neuron.this, s"_connect(${destinationRef.id},$weight)")
    synapses += new Synapse(destinationRef, weight)
    sender ! Success("connect_"+id)
  }
  protected def connect(destinationRef: NeuronRef, weight: Double):Unit = findSynapse(destinationRef.id) match {
    case Some(s) => sender ! Failure(s"a synapse to ${destinationRef.id} already exists")
    case None => _connect(destinationRef, weight)
  }
  
  protected def disconnect(destinationId: String):Unit = findSynapse(destinationId) match {
    case Some(s) => synapses -= s
    case None =>
  }
  protected def disconnect(destination: Neuron):Unit = disconnect(destination.id)
  
  def findSynapse(destinationId: String):Option[Synapse] = synapses.find(_.dest.id == destinationId)
  def findSynapse(destination: Neuron):Option[Synapse] = findSynapse(destination.id)

  private def answer(msg: Answer) = NetRef.get match {
    case Some(netref) => netref ! msg
    case None => //error(this, "answer demanded, but no netref!")
  }
  
  protected def init(){
    debug(Neuron.this, s"init for $id with threshold $treshold and slope $slope")
    addTresholdPassedTrigger("run", (_: Neuron) => future { 
      Thread.sleep(50L)
      debug("tresholdPassedTrigger run")
      that.run() 
    })
    answer(Success("init_"+Neuron.this.id))
  }
  
  private def shutdown(){
    answer(NeuronShutdownDone(id))
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
