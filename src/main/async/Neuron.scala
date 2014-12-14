package main.async

import akka.actor._
import scala.collection.mutable
import main.async.logger.LOG
import main.async.logger.LOG._
import main.utils.Utils._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent._
import scala.concurrent.duration._
import Messages._
import Context.tickTime

import ExecutionContext.Implicits.global

case class HushValue(iterations: Int = 1) extends AnyVal
  
sealed trait ForgetTrait extends Any
case class ForgetValue(value: Double) extends AnyVal with ForgetTrait 
case object ForgetAll extends ForgetTrait
case object DontForget extends ForgetTrait

class Neuron(
    val id: String, 
    val threshold: Double, 
    val slope: Double, 
    val hushValue: HushValue, 
    val forgetting: ForgetTrait,
    private var synapses: List[Synapse] = List[Synapse]()
) extends Actor with NeuronTriggers {
  implicit val that = this
  
  protected var buffer = 0.0
  def input = buffer // only for debugging purposes
  var lastOutput = 0.0 // only for debugging purposes
  
  def silence(){
    LOG += s"$id silence, hushValue.iterations is ${hushValue.iterations}"
    buffer = 0.0
    if(hushValue.iterations == 0) makeSleep() else makeHush()
  }
  
  private def makeSleep() = {
    context.become(sleep)
    context.system.scheduler.scheduleOnce(tickTime millis){ self ! WakeUp }
  }
  
  private def makeHush() = {
    val t = tickTime * hushValue.iterations
    LOG += s"$id making hush for ${hushValue.iterations} iterations ($t millis)"
    context.become(hushTime)
    context.system.scheduler.scheduleOnce(t millis){ self ! WakeFromHush }
  }
  
  protected def calculateOutput:Double = f(buffer, slope)
  
  def +=(signal: Double){
    forget()
    LOG += s"$id adding signal $signal to buffer $buffer, threshold is $threshold"
    buffer += signal
    tick()
  }
  
  private def wakeUp(){
    LOG += s"$id waking up"
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
      val delta = offset.toDouble/tickTime * forgetValue
      LOG += s"forgetting, offset=$offset, sleepTime=$tickTime, forgetValue=$forgetValue, so delta is $delta"
      buffer = if(buffer > 0.0) Math.max(buffer - delta, 0.0) else Math.min(buffer + delta, 0.0)
      lastForgetting = Some(System.currentTimeMillis())
    case _ =>
  }
  
  protected def run(){
    val output = calculateOutput
    lastOutput = output
    buffer = 0.0
    if(synapses.nonEmpty){
      LOG += s"$id trigger output $output, synapses size: ${synapses.size}"
      synapses.foreach( _.send(output) )
    } 
    afterFireTriggers.values.foreach( _() )
    makeSleep()
  }
    
  def findSynapse(destinationId: String):Option[Synapse] = 
    if(synapses.nonEmpty) synapses.find(_.dest.id == destinationId) else None
  def findSynapse(destination: Neuron):Option[Synapse] = findSynapse(destination.id)

  protected def answer(msg: Answer) = NetRef.get match {
    case Some(netref) => netref ! msg
    case None => //error(this, "answer demanded, but no netref!")
  }
  
  protected def init(){
    LOG += s"init for $id with threshold $threshold and slope $slope"
    
    addTresholdPassedTrigger("run", () => that.run() )  
    answer(Success("init_"+this.id))
  }
  
  private def shutdown(){
    answer(NeuronShutdownDone(id))
    context.stop(self)
  }
  
  def receive = activeBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, active")
  
  def sleep = sleepBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, sleep")

  def hushTime = hushBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, sleep")
  
  val activeBehaviour: Receive = {
    case Signal(s) => this += s
    case HushNow => silence()
    case WakeUp =>
  }
  
  val hushBehaviour: Receive = {
    case WakeFromHush if sender == self => 
      LOG +=  s"$id hush wake up"
      wakeUp()
    case Signal(s) => LOG += s"$id, signal hushed: $s" // so it's like sleep, but we ignore signals
  }
  
  val sleepBehaviour: Receive = {
    case WakeUp if sender == self => 
      LOG +=  s"$id sleep wake up" 
      wakeUp()
    case HushNow => silence()
    case Signal(s) => buffer += s
  }
   
  val commonBehaviour: Receive = {
      case GetId => sender ! Msg(0.0, id)
      case GetInput => sender ! Msg(input.toDouble, id)
      case FindSynapse(destinationId) => sender ! MsgSynapse(findSynapse(destinationId))
      case GetSynapses => sender ! MsgSynapses(synapses)
      case SetSynapses(synapses) => this.synapses = synapses
      case NeuronShutdown => shutdown()
      case AddAfterFireTrigger(triggerId, trigger) => 
        addAfterFireTrigger(triggerId, trigger)
        sender ! Success(triggerId)
      case RemoveAfterFireTrigger(triggerId) =>
        removeAfterFireTrigger(triggerId)
        sender ! Success(triggerId)
      case Init => init()
      case ResetBuffer => buffer = 0.0
  }
  
  def otherBehaviour(state: String): Receive = {
    case other => LOG += s"$state, unrecognized message: $other"
  }
}
