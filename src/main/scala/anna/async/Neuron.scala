package anna.async

import akka.actor._
import anna.Context
import anna.async.Messages._
import anna.data.{ForgetAll, ForgetTrait, ForgetValue, HushValue}
import anna.logger.LOG
import anna.utils.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class Neuron(
    val id: String,
    val netId: String,
    val threshold: Double, 
    val slope: Double, 
    val hushValue: HushValue, 
    val forgetting: ForgetTrait,
    val tickTimeMultiplier: Double,
    protected val f:(Double,Double)=>Double,
protected var synapses: Seq[Synapse] = Seq[Synapse]()
) extends Actor with NeuronTriggers {
  implicit val that = this
  
  protected var buffer = 0.0
  protected var lastOutput = 0.0

  private val schedulerBuffer = new SchedulerBuffer(context)

  override def preStart():Unit = {
    NeuronCounter.reg(netId, id, self)
  }

  override def postStop():Unit = {
    schedulerBuffer.clear()
    NeuronCounter.unreg(netId, id)
  }

  private def hushNow(){
    LOG += s"$id hushNow, hushValue.iterations is ${hushValue.iterations}"
    buffer = 0.0
    if(hushValue.iterations == 0) makeSleep() else makeHush()
    triggerHushRequested()
  }
  
  private def makeSleep() = {
    context.become(sleep)
    schedulerBuffer.schedule((tickTimeMultiplier * Context().tickTime).toLong millis){ self ! WakeUp }
  }
  
  private def makeHush() = {
    val t = (tickTimeMultiplier * Context().tickTime).toLong * hushValue.iterations
    LOG += s"$id making hush for ${hushValue.iterations} iterations ($t millis)"
    context.become(hushTime)
    schedulerBuffer.schedule(t millis){ self ! WakeFromHush }
  }
  
  protected def calculateOutput:Double = f(buffer, slope)
  
  protected def +=(signal: Double){
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
    buffer = Utils.minmax(-1.0, buffer, 1.0)
    if(buffer > threshold){
      triggerThresholdPassed()
      run()
    }
    if(forgetting == ForgetAll()) buffer = 0.0
  }
  
  private var lastForgetting:Option[Long] = None
  
  private def forget() = forgetting match {
    case ForgetValue(_) if lastForgetting == None => lastForgetting = Some(System.currentTimeMillis())
    case ForgetValue(forgetValue) =>
      val offset = System.currentTimeMillis() - lastForgetting.get
      val delta = offset.toDouble/(tickTimeMultiplier * Context().tickTime) * forgetValue
      LOG += s"forgetting, offset=$offset, sleepTime=${tickTimeMultiplier * Context().tickTime}, forgetValue=$forgetValue, so delta is $delta"
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
    triggerAfterFire(output)
    makeSleep()
  }
    
  protected def findSynapse(destinationId: String):Option[Synapse] = 
    if(synapses.nonEmpty) synapses.find(_.dest.id == destinationId) else None
  protected def findSynapse(destination: Neuron):Option[Synapse] = findSynapse(destination.id)

  protected def answer(msg: Answer) = NetRef.get match {
    case Some(netref) => netref ! msg
    case None =>
  }

  private def reset(): Unit ={
    buffer = 0.0
    context.become(receive)
    schedulerBuffer.clear()
    answer(Success(id))
  }

  private def removeTriggers(): Unit ={
    removeAllTriggers()
    answer(Success(id))
  }
  
  def receive = activeBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, active")
  
  def sleep = sleepBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, sleep")

  def hushTime = hushBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, sleep")
  
  val activeBehaviour: Receive = {
    case Signal(s) => this += s
    case HushNow => hushNow()
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
    case HushNow => hushNow()
    case Signal(s) => buffer += s
  }
   
  val commonBehaviour: Receive = {
      case GetId => sender ! Msg(0.0, id)
      case GetInput => sender ! Msg(buffer, id)
      case GetLastOutput => sender ! Msg(lastOutput, id)
      case FindSynapse(destinationId) => sender ! MsgSynapse(findSynapse(destinationId))
      case GetSynapses => sender ! MsgSynapses(synapses)
      case SetSynapses(synapses) => this.synapses = synapses
      case AddAfterFireTrigger(triggerId, trigger) => 
        addAfterFire(triggerId, trigger)
        sender ! Success(triggerId)
      case RemoveAfterFireTrigger(triggerId) =>
        removeAfterFire(triggerId)
        sender ! Success(triggerId)
      case AddHushRequestedTrigger(triggerId, trigger) => 
        addHushRequested(triggerId, trigger)
        sender ! Success(triggerId)
      case RemoveHushRequestedTrigger(triggerId) =>
        removeHushRequested(triggerId)
        sender ! Success(triggerId)
      case RemoveAllTriggers => removeTriggers()
      case Reset => reset()
  }
  
  def otherBehaviour(state: String): Receive = {
    case other => LOG += s"$state, unrecognized message: $other"
  }
}
