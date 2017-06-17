package anna.async

import akka.actor._
import anna.async.Messages._
import anna.data.NeuronData

import scala.collection.mutable

class Net(val id: String) extends Actor {
  private val neurons = mutable.ListBuffer[NeuronRef]()
  private var inputs = List[NeuronRef]()

  def receive: Receive = {
    case GetId => sender ! Msg(0.0, id)
    case GetNeurons => sender ! MsgNeurons(neurons.toList)
    case CreateNeuron(data) => createNeuron(data)
    case Shutdown => shutdown()
    case GetInputs => sender ! MsgNeurons(inputs)
    case SetInputs(ids) => setInputs(ids.toSet)
    case GetNeuron(id) => sender ! MsgNeuron(findRef(id))
    case SignalList(in) => signal(in)
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
    case NeuronTypeStandard() => createStandard(data.id, data.threshold, data.silenceIterations)
    case NeuronTypeDummy() => createDummy(data.id, data.silenceIterations)
    case NeuronTypeSilencing() => createSilencing(data.id)
  }

  private def createStandard(id: String, threshold: Double, silenceIterations: Int) = {
	  val ref = context.actorOf(Props(new Neuron(id, this.id, threshold, silenceIterations)))
    add(id, ref)
  }

  private def createDummy(id: String, silenceIterations: Int) = {
	  val ref = context.actorOf(Props(new DummyNeuron(id, this.id)))
    add(id, ref)
  }

  private def createSilencing(id: String) = {
    val ref = context.actorOf(Props(new SilencingNeuron(id, this.id)))
    add(id, ref)
  }

  private def signal(in: List[Double]){
    assert(in.size == inputs.size, s"Difference in size between the input layer (${inputs.size}) and the input (${in.size})")
    inputs.zip(in).foreach {
      case (inputNeuron: NeuronRef, signal: Double) => inputNeuron ! Signal(signal, "Input")
    }
  }

  private def setInputs(ids: Set[String]){
    inputs = neurons.filter( n => ids.contains(n.id) ).toList.sortBy(_.id)
    if(inputs.size != ids.size){
      val inIds = inputs.map( _.id )
      val notFound = ids.filterNot( inIds.contains(_) )
      sender ! Failure(s"setInputs, unable to find neurons with ids: $notFound")
    } else {
      sender ! Success("setInputLayer_" + id)
    }
  }

  private def findRef(id: String):Option[NeuronRef] = neurons.find(_.id == id)
}


