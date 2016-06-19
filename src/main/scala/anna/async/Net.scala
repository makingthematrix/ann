package anna.async

import akka.actor._
import anna.async.Messages._
import anna.data.{ForgetTrait, HushValue, NeuronData}
import anna.logger.LOG

import scala.collection.mutable

class Net(val id: String) extends Actor {
  private val neurons = mutable.ListBuffer[NeuronRef]()
  private var ins = List[NeuronRef]()

  def receive: Receive = {
    case GetId => sender ! Msg(0.0, id)
    case GetNeurons => sender ! MsgNeurons(neurons.toList)
    case CreateNeuron(data) => createNeuron(data)
    case Shutdown => shutdown()
    case GetInputs => sender ! MsgNeurons(inputs)
    case SetInputs(ids) => setInputs(ids)
    case GetNeuron(id) => sender ! MsgNeuron(findRef(id))
    case SignalSeq(in) => signal(in)
    case Reset => resetBuffer()
    case RemoveAllTriggers => removeTriggers()
    case RemoveAfterFireTrigger(id) => removeAfterFire(id)
    case Terminated(actorRef) =>
      neurons -= neurons.find( _.ref == actorRef ).get
      if(neurons.isEmpty) self ! PoisonPill
  }

  private var shutdownCallerOpt:Option[ActorRef] = None

  override def preStart():Unit = {

  }

  override def postStop():Unit = {
    if(shutdownCallerOpt != None) shutdownCallerOpt.get ! NetShutdownDone(id)
  }

  private def shutdown() = {
    shutdownCallerOpt = Some(sender)
    if(neurons.nonEmpty) {
      neurons.foreach(_ ! PoisonPill)
    } else {
      self ! PoisonPill
    }
  }

  def waiting(caller: ActorRef, waitingFor: Set[String], title: String = ""): Receive = {
    case Success(id) =>
      val newWaitingFor = waitingFor - id
      if(newWaitingFor.isEmpty){
        caller ! Success(id)
        context.become(receive)
      } else context.become(waiting(caller, newWaitingFor, title))
  }

  private def inputs = ins.toList

  private def middles = {
    val inputIds = ins.map( _.id ).toSet
    neurons.filterNot( n => inputIds.contains(n.id) ).toList
  }

  private def remove(id: String) = findRef(id) match {
    case Some(ref) => neurons -= ref
    case None =>
  }

  private def resetBuffer() = {
    context.become( waiting(sender, neurons.map(_.id).toSet, "resetting") )
    neurons.foreach(_ ! Reset)
  }

  private def removeAfterFire(id:String) = {
    context.become( waiting(sender, neurons.map(_.id).toSet, s"removing an after fire trigger $id") )
    neurons.foreach(_.removeAfterFire(id))
  }

  private def removeTriggers() = {
    context.become( waiting(sender, neurons.map(_.id).toSet, "removing triggers") )
    neurons.foreach(_ ! RemoveAllTriggers)
  }

  private def add(id: String, ref: ActorRef) = {
    val neuronRef = new NeuronRef(id, ref)
    neurons += neuronRef
    context.watch(ref)
    sender ! neuronRef
  }

  private def createNeuron(data:NeuronData) = data.neuronType match {
    case NeuronTypeStandard() => createStandard(data.id, data.threshold, data.hushValue, data.forgetting, data.activationFunctionName)
    case NeuronTypeDummy() => createDummy(data.id, data.hushValue)
    case NeuronTypeHush() => createHush(data.id)
  }

  private def createStandard(id: String,
                           threshold: Double,
                           hushValue: HushValue,
                           forgetting: ForgetTrait,
                           activationFunctionName: String
                           ) = {
    val f = ActivationFunction(activationFunctionName)
	  val ref = context.actorOf(Props(new Neuron(id, this.id, threshold, hushValue, forgetting, f)))
    add(id, ref)
  }

  private def createDummy(id: String, hushValue: HushValue) = {
	  val ref = context.actorOf(Props(new DummyNeuron(id, this.id)))
    add(id, ref)
  }

  private def createHush(id: String) = {
    val ref = context.actorOf(Props(new HushNeuron(id, this.id)))
    add(id, ref)
  }

  private def signal(in: Seq[Double]){
    assert(in.size == ins.size, s"Difference in size between the input layer (${ins.size}) and the input (${in.size})")
    ins.zip(in).foreach( tuple => tuple._1 += tuple._2 )
  }

  private def setInputs(ids: Seq[String]){
    ins = neurons.filter( n => ids.contains(n.id) ).toList
    if(ins.size != ids.size){
      val inIds = ins.map( _.id )
      val notFound = ids.filterNot( inIds.contains(_) )
      sender ! Failure(s"setInputs, unable to find neurons with ids: $notFound")
    } else {
      sender ! Success("setInputLayer_" + id)
    }
  }

  private def findRef(id: String):Option[NeuronRef] = neurons.find(_.id == id)
}


