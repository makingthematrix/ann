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
import Messages._

import ExecutionContext.Implicits.global

case class HushValue(value: Double =0.0, iterations: Int = 1)

class Neuron(val id: String, val threshold: Double, val slope: Double, val hushValue: HushValue, private var forgetting: ForgettingTick)
extends Actor with NeuronTriggers {
  protected val synapses = mutable.ListBuffer[Synapse]()
  
  def that = this
  
  protected var buffer = 0.0
  protected var output = 0.0
  
  def input = buffer // only for debugging purposes
  def lastOutput = output // only for debugging purposes
  
  def silence(){
    debug(this,s"$id silence, hushValue.iterations is ${hushValue.iterations}")
    assert(hushValue.value <= 0.0, s"The hush value for $id is $hushValue - expected something <= 0.0")
    buffer = hushValue.value
    output = 0.0
    if(hushValue.iterations == 0) makeSleep() else makeHush()
  }
  
  private def makeSleep() = {
    context.become(sleep)
    context.system.scheduler.scheduleOnce(Context.sleepTime millis){ self ! WakeUp }
  }
  
  private def makeHush() = {
    val t = Context.sleepTime * hushValue.iterations.toLong
    debug(this, s"$id making hush for ${hushValue.iterations} iterations ($t millis)")
    context.become(hushTime)
    context.system.scheduler.scheduleOnce(t millis){ self ! WakeFromHush }
  }
  
  protected def calculateOutput:Double = f(buffer, slope)
  
  def +=(signal: Double){
    forget()
    debug(this, s"$id adding signal $signal to buffer $buffer, threshold is $threshold")
    buffer += signal
    tick()
  }
  
  private def wakeUp(){
    debug(this,s"$id waking up")
    forget()
	tick()
    context.become(receive)
  }
  
  private def tick(){
    buffer = minmax(-1.0, buffer, 1.0)
    if(buffer > threshold) tresholdPassedTriggers.values.foreach( _() )
    if(forgetting == ForgetAll) buffer = 0.0
  }
  
  private var lastForgetting:Option[Long] = None
  
  private def forget() = forgetting match {
    case ForgetValue(_) if lastForgetting == None => lastForgetting = Some(System.currentTimeMillis())
    case ForgetValue(forgetValue) =>
      val offset = System.currentTimeMillis() - lastForgetting.get
      val delta = offset.toDouble/Context.sleepTime * forgetValue
      debug(this, s"forgetting, offset=$offset, sleepTime=${Context.sleepTime}, forgetValue=$forgetValue, so delta is $delta")
      buffer = if(buffer > 0.0) Math.max(buffer - delta, 0.0) else Math.min(buffer + delta, 0.0)
      lastForgetting = Some(System.currentTimeMillis())
    case _ =>
  }
  
  protected def run(){
    output = calculateOutput
    buffer = 0.0
    debug(this, s"$id trigger output $output, synapses size: ${synapses.size}")
    synapses.foreach( _.send(output) )
    afterFireTriggers.values.foreach( _() )
    makeSleep()
  }
  
  def connect(destination: Neuron, weight: Double) =
    throw new IllegalArgumentException("Use Connect(destinationRef: NeuronRef, weight: Double) request")
  
  private def _connect(destinationRef: NeuronRef, weight: SynapseWeight) = {
    //debug(this, s"_connect(${destinationRef.id},$weight)")
    synapses += new Synapse(destinationRef, weight)
    sender ! Success("connect_"+id)
  }
  protected def connect(destinationRef: NeuronRef, weight: SynapseWeight):Unit = findSynapse(destinationRef.id) match {
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

  protected def answer(msg: Answer) = NetRef.get match {
    case Some(netref) => netref ! msg
    case None => //error(this, "answer demanded, but no netref!")
  }
  
  protected def init(usePresleep: Boolean){
    debug(this, s"init for $id with threshold $threshold and slope $slope")
    
    addTresholdPassedTrigger("run", () => that.run() )
    
    //if(usePresleep) context.become(presleep)
    
    answer(Success("init_"+this.id))
  }
  
  private def shutdown(){
    answer(NeuronShutdownDone(id))
    context.stop(self)
  }
  
  private def setForgetting(forgetting: ForgettingTick){
    this.forgetting = forgetting
    sender ! Success(id)
  }
  
  def receive = activeBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, active")
  
  def sleep = sleepBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, sleep")

  def hushTime = hushBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, sleep")
  
  //def presleep = preSleepBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, presleep")

  val activeBehaviour: Receive = {
    case Signal(s) => this += s
    case HushNow => silence()
    case WakeUp =>
  }
  
  val hushBehaviour: Receive = {
    case WakeFromHush if sender == self => debug(this, s"$id hush wake up"); wakeUp()
    case Signal(s) => debug(this,s"$id, signal hushed: $s")// so it's like sleep, but we ignore signals
  }
  
  val sleepBehaviour: Receive = {
    case WakeUp if sender == self => debug(this, s"$id sleep wake up"); wakeUp()
    case HushNow => silence()
    case Signal(s) => buffer += s
  }
  
  val commonBehaviour: Receive = {
      case GetId => sender ! Msg(0.0, id)
      case GetInput => sender ! Msg(input.toDouble, id)
      case GetLastOutput => sender ! Msg(lastOutput.toDouble, id)
      case FindSynapse(destinationId) => sender ! MsgSynapse(findSynapse(destinationId))
      case GetSynapses => sender ! MsgSynapses(synapses.toList)
      case NeuronShutdown => shutdown()
      case Connect(destinationRef, weight) => connect(destinationRef, weight)
      case Disconnect(destinationId) => disconnect(destinationId)
      case AddAfterFireTrigger(triggerId, trigger) => 
        addAfterFireTrigger(triggerId, trigger)
        sender ! Success(triggerId)
      case RemoveAfterFireTrigger(triggerId) =>
        removeAfterFireTrigger(triggerId)
        sender ! Success(triggerId)
      case Init(usePresleep) => init(usePresleep)
      case SetForgetting(forgetting) => setForgetting(forgetting)
  }
  
  /*val preSleepBehaviour: Receive = {
    case WakeUp => wakeUp()
    case Signal(s) => 
      buffer += s
      context.system.scheduler.scheduleOnce(Context.sleepTime millis){ 
        wakeUp()
      }
  }*/
  
  def otherBehaviour(state: String): Receive = {
    case other => debug(this,s"$state, unrecognized message: $other")
  }
}
