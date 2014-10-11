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

case class Synapse(val dest: NeuronRef,val weight: Double){
  def send(signal: Double) = dest ! Signal(signal * weight)
}

class Neuron(val id: String, val treshold: Double, val slope: Double, val forgetting: ForgettingTick)
extends Actor with NeuronTriggers[Neuron] {
  protected val synapses = mutable.ListBuffer[Synapse]()
  
  def this(id: String) = this(id, 0.5, 20.0, DontForget)
  def this(id: String, treshold: Double) = this(id, treshold, 20.0, DontForget)
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
  
  def +=(signal: Double) = {
    debug(this, s"$id adding signal $signal to buffer $buffer, treshold is $treshold")
    buffer = minmax(-1.0, buffer+signal, 1.0)
    if(buffer > treshold) tresholdPassedTriggers.values.foreach( _(this) )
    if(forgetting == ForgetAll) buffer = 0.0
  }
  
  protected def run() = {
    output = calculateOutput
    buffer = 0.0
    debug(this, s"$id trigger output $output, synapses size: ${synapses.size}")
    synapses.foreach( _.send(output) )
    afterFireTriggers.values.foreach( _(this) )
    debug(this, s"$id, going to sleep")
    context.become(sleep)
    context.system.scheduler.scheduleOnce(50 millis){ 
      self ! WakeUp
    }
  }
  
  def connect(destination: Neuron, weight: Double) =
    throw new IllegalArgumentException("Use Connect(destinationRef: NeuronRef, weight: Double) request")
  
  private def _connect(destinationRef: NeuronRef, weight: Double) = {
    //debug(this, s"_connect(${destinationRef.id},$weight)")
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
    addTresholdPassedTrigger("run", (_: Neuron) => that.run() )
    context.become(presleep)
    forgetting match {
      case ForgetValue(value) => context.system.scheduler.schedule(50 millis, 50 millis){
        self ! Forgetting
      }
      case _ =>
    }
    answer(Success("init_"+this.id))
  }
  
  private def shutdown(){
    answer(NeuronShutdownDone(id))
    context.stop(self)
  }
  
  private def wakeUp(){
    debug(this,s"$id, waking up")
    buffer = minmax(-1.0, buffer, 1.0)
    if(buffer > treshold) tresholdPassedTriggers.values.foreach( _(this) )
    context.unbecome()
  }
  
  private def forget() = forgetting match {
    case ForgetAll => silence()
    case ForgetValue(value) if value > 0 => {
      if(buffer > 0.0) buffer = Math.max(buffer - value, 0.0)
      else if(buffer < 0.0) buffer = Math.min(buffer + value, 0.0)
      debug(this,s"$id, forgetting $value")
      // might be changed into the S function later on      
    }
    case DontForget =>
  }
  
  def receive = activeBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, active")
  
  def sleep = sleepBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, sleep")

  def presleep = preSleepBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, presleep")

  val activeBehaviour: Receive = {
    case WakeUp =>
    case Signal(s) => this += s
  }
  
  val commonBehaviour: Receive = {
      case GetId => sender ! Msg(0.0, id)
      case GetInput => sender ! Msg(input.toDouble, id)
      case GetLastOutput => sender ! Msg(lastOutput.toDouble, id)
      case HushNow => silence()
      case FindSynapse(destinationId) => sender ! MsgSynapse(findSynapse(destinationId))
      case GetSynapses => sender ! MsgSynapses(synapses.toList)
      case NeuronShutdown => shutdown()
      case Connect(destinationRef, weight) => connect(destinationRef, weight)
      case Disconnect(destinationId) => disconnect(destinationId)
      case AddAfterFireTrigger(id, f) => addAfterFireTrigger(id, f)
      case Init => init()
      case Forgetting => forget()
  }
  
  val sleepBehaviour: Receive = {
    case WakeUp => wakeUp()
    case Signal(s) => buffer += s
  }
  
  val preSleepBehaviour: Receive = {
    case WakeUp => wakeUp()
    case Signal(s) => 
      buffer += s
      context.system.scheduler.scheduleOnce(50 millis){ 
        wakeUp()
        forget()
      }
  }
  
  def otherBehaviour(state: String): Receive = {
    case other => debug(this,s"$state, unrecognized message: $other")
  }
}
