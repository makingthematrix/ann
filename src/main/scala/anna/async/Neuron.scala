package anna.async

import akka.actor._
import anna.Context
import anna.async.Messages._
import anna.data._
import anna.logger.LOG
import anna.utils.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class Neuron(
    val id: String,
    val netId: String,
    val threshold: Double,
    val hushValue: HushValue, 
    val forgetting: ForgetTrait,
    protected val f:(Double,Double)=>Double,
    protected var synapses: List[Synapse] = List[Synapse](),
    protected var friends: Set[String] = Set[String]()
) extends Actor with NeuronTriggers {
  implicit val that = this
  
  protected var buffer = 0.0

  // statistics only
  protected var highestBuffer = 0.0
  protected var lastOutput = 0.0

  private val schedulerBuffer = new SchedulerBuffer(context)

  private var isSleeping = false

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
    LOG += s"$id: buffer is now $buffer"
    makeHush()
    triggerHushRequested()
  }
  
  private def makeSleep() = {
    LOG += s"$id going to sleep for ${Context().tickTime} ms"
    isSleeping = true
    context.system.scheduler.scheduleOnce(Context().tickTime millis){ wakeUp() }
  }
  
  private def makeHush() = {
    val t = Context().tickTime * hushValue.iterations
    if(t > 0){
      LOG += s"$id making hush for ${hushValue.iterations} iterations ($t millis)"
      context.become(hushTime)
      schedulerBuffer.schedule(t millis){ wakeFromHush() }
    }
  }

  private def wakeFromHush() = {
    LOG += s"$id waking up from hush"
    isSleeping = false
    context.become(receive)
  }
  
  protected def calculateOutput:Double = f(buffer, 0.0)
  
  protected def +=(signal: Double){
    LOG += s"$id adding signal $signal to buffer $buffer, threshold is $threshold, forgetting is $forgetting"
    buffer += signal
    LOG += s"$id: buffer is now $buffer"
    if(!isSleeping){
      tick()
    }
  }
  
  private def wakeUp(){
    LOG += s"$id waking up"
    isSleeping = false
    tick()
  }

  private def biggerOrCloseEnough(x: Double, y: Double) = { x > 0.0 && x + 0.001 > y }

  private def tick():Unit = this.synchronized {
    forget()
    buffer = Utils.minmax(-1.0, buffer, 1.0)
    if (highestBuffer < buffer) highestBuffer = buffer
    if (biggerOrCloseEnough(buffer, threshold)) {
      triggerThresholdPassed()
      run()
    }
    if (forgetting == ForgetAll()) buffer = 0.0
    LOG += s"after the tick: $id: buffer is now $buffer"
  }

  private var lastForgetting:Option[Long] = None
  
  private def forget() = forgetting match {
    case ForgetValue(_) if lastForgetting == None => lastForgetting = Some(System.currentTimeMillis())
    case ForgetValue(forgetValue) =>
      val offset = System.currentTimeMillis() - lastForgetting.get
      val delta = (offset.toDouble/Context().tickTime) * forgetValue
      LOG += s"forgetting, offset=$offset, sleepTime=${Context().tickTime}, forgetValue=$forgetValue, so delta is $delta"
      buffer = if(buffer > 0.0) Math.max(buffer - delta, 0.0) else Math.min(buffer + delta, 0.0)
      lastForgetting = Some(System.currentTimeMillis())
    case _ =>
  }
  
  protected def run(): Unit = {
    val output = calculateOutput
    lastOutput = output
    buffer = 0.0

    LOG += s"$id trigger output $output, synapses size: ${synapses.size}"
    makeSleep()
    synapses.foreach( _.send(output, id) )

    triggerAfterFire(output)
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
    LOG += s"reset; $id: buffer is now $buffer"

    context.become(receive)
    schedulerBuffer.clear()
    answer(Success(id))
  }

  private def removeTriggers(): Unit ={
    removeAllTriggers()
    answer(Success(id))
  }
  
  def receive = activeBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, active")

  def hushTime = hushBehaviour orElse commonBehaviour orElse otherBehaviour(s"$id, hushTime")
  
  val activeBehaviour: Receive = {
    case Signal(s, _) => this += s
    case HushRequest => hushNow()
  }
  
  val hushBehaviour: Receive = {
    case WakeFromHush => wakeFromHush()
    case Signal(s, senderId) =>
      LOG += s"$id, signal during hush. friends: $friends, sender: $senderId"
      if(friends.contains(senderId)){
        LOG += s"$id, waking up from being hushed because a friend called"
        wakeFromHush()
        this += s
      } else {
        LOG += s"$id, signal hushed: $s" // so it's like sleep, but we ignore signals
      }
  }

  val commonBehaviour: Receive = {
      case GetId => sender ! Msg(0.0, id)
      case GetData => sender ! info
      case FindSynapse(destinationId) => sender ! MsgSynapse(findSynapse(destinationId))
      case GetSynapses => sender ! MsgSynapses(synapses)
      case SetSynapses(synapses) => this.synapses = synapses.toList
      case SetFriends(friends) => this.friends = friends
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

  def info = NeuronInfo(
    id, netId, threshold, hushValue, forgetting,
    synapses.map(_.info), buffer, highestBuffer, lastOutput
  )
}
