package main.async

import akka.actor._
import scala.collection.mutable
import scala.concurrent.Await
import akka.util.Timeout
import scala.concurrent.duration._

import main.async.logger.LOG._
import Messages._

class Net(val id: String) extends Actor {
  private val neurons = mutable.ListBuffer[NeuronRef]()
  private val ins = mutable.ListBuffer[NeuronRef]()
  
  def inputLayer = ins.toSeq
  def middleLayer = {
    val inputIds = ins.map( _.id ).toSet
    neurons.filterNot( n => inputIds.contains(n.id) )
  }

  private def remove(id: String) = findRef(id) match {
    case Some(ref) => neurons -= ref
    case None => 
  }
  
  private var caller: Option[ActorRef] = None
  
  def shutdowning: Receive = {
    case NeuronShutdownDone(id) => {
      remove(id)
      if(neurons.isEmpty){
        if(caller != None) caller.get ! NetShutdownDone(id) 
        caller = None
        context.stop(self)
      }
    }
  }
  
  private val waitingForInit = mutable.Set[String]()
  
  def initializing: Receive = {
    case Success(initId) if initId.startsWith("init_") => {
      debug(this, "initializing, " + initId)
      val id = initId.substring(5)
      waitingForInit.remove(id)
      if(waitingForInit.isEmpty) {
        if(caller != None) caller.get ! Success("netinit_"+this.id)
        caller = None
        context.unbecome
      }
    }
    case Success(str) => error(this, "this Success message shouldn't be here: " + str)
    case Failure(initId) if initId.startsWith("init_") => {
      error(this, initId) 
      val id = initId.substring(5)
      if(caller != None) caller.get ! Success("netinit_"+this.id)
      caller = None
      context.unbecome
    }
    case Failure(str) => error(Net.this, "this Failure message shouldn't be here: " + str)
  }
  
  private def init() = {
    debug(this, s"init for $id")
    waitingForInit ++= neurons.map( _.id )
    caller = Some(sender)
    context.become(initializing)
    neurons.foreach( _ ! Init )
  }
  
  private def shutdown() = {
    debug(s"shutdown for $id")
    caller = Some(sender)
    context.become(shutdowning)
    neurons.foreach(_ ! NeuronShutdown)
  }
  
  private def add(id: String, ref: ActorRef) = {
    val neuronRef = new NeuronRef(id, ref)
    neurons += neuronRef
    sender ! neuronRef
  }
  
  private def createNeuron(id: String, treshold: Double, slope: Double, hushValue: HushValue, forgetting: ForgetTrait){
	val ref = context.actorOf(Props(new Neuron(id, treshold, slope, hushValue, forgetting)), name=id)
    add(id, ref)
  }
  
  private def createDummy(id: String, hushValue: HushValue){
	val ref = context.actorOf(Props(new DummyNeuron(id, hushValue)), name=id)
    add(id, ref)
  }

  def receive: Receive = {
    case Init => init()
    case GetId => sender ! Msg(0.0, id)
    case GetNeurons => sender ! MsgNeurons(neurons.toList)
    case CreateNeuron(id, threshold, slope, hushValue, forgetting) => createNeuron(id, threshold, slope, hushValue, forgetting)
    case CreateDummy(id, hushValue) => createDummy(id, hushValue)
    case ConnectNeurons(id1, id2, weight) => connectNeurons(id1, id2, weight) 
    case Shutdown => shutdown()
    case GetInputLayer => sender ! MsgNeurons(inputLayer.toList)
    case SetInputLayer(ids) => setInputLayer(ids)
    case GetMiddleLayer => sender ! MsgNeurons(middleLayer.toList)
    case GetNeuron(id) => getNeuron(id)
    case SignalSeq(in) => signal(in)
  }
  
  private def signal(in: Seq[Double]){
    assert(in.size == ins.size, s"Difference in size between the input layer (${ins.size}) and the input (${in.size})")
    debug(this, s"signal received in $id: " + in.mkString(", "))
    ins.zip(in).foreach( tuple => tuple._1 += tuple._2 )
  }
  
  private def setInputLayer(ids: Seq[String]){
    debug(Net.this, s"input layer set in $id: " + ids.mkString(", "))
    ins.clear
    neurons.filter( n => ids.contains(n.id) ).foreach( ins += _ )
    sender ! Success("setInputLayer_"+id)
  }
  
  private def connectNeurons(id1: String, id2: String, weight: Double) = findRef(id1, id2) match {
    case (Some(ref1),Some(ref2)) => {
      debug(this, s"connectNeurons($id1,$id2,$weight in $id)")
      ref1.connect(ref2, weight) match {
        case true => sender ! Success(s"${id}_connectNeurons($id1,$id2)")
        case false => sender ! Failure(s"$id: Unable to connecct neurons $id1 and $id2")
      }
    }
    case (Some(ref1),None) => sender ! Failure(s"There is no neuron with id $id2")
    case (None,Some(ref2)) => sender ! Failure(s"There is no neuron with id $id1")
    case (None, None) => sender ! Failure(s"There is neither neuron with id $id1 nor $id2")
  }
  
  private def findRef(id: String):Option[NeuronRef] = neurons.find(_.id == id)
  private def findRef(id1: String, id2: String):(Option[NeuronRef],Option[NeuronRef]) = (findRef(id1), findRef(id2))
  
  private def getNeuron(id: String) = sender ! MsgNeuron(findRef(id))
  
  def size = inputLayer.size + middleLayer.size
  def inputSize = inputLayer.size
  def middleSize = middleLayer.size
  
  def ids = inputIds ++ middleIds
  def inputIds = inputLayer.map( _.id )
  def middleIds = middleLayer.map( _.id )
  
  def find(id: String):Option[NeuronRef] = {
    debug(this, s"id in ${this.id}")
    val inFind = inputLayer.find( _.id == id )
    if(inFind.isDefined) return inFind
    middleLayer.find( _.id == id )
  }
  
  protected def find(id1: String, id2: String):(NeuronRef,NeuronRef) = {
    val n1 = find(id1)
    if(n1.isEmpty) throw new IllegalArgumentException("There is no neuron with id " + id1)
    val n2 = find(id2)
    if(n2.isEmpty) throw new IllegalArgumentException("There is no neuron with id " + id2)
    (n1.get,n2.get)
  }
  
  def contains(id: String) = find(id).isDefined
  
  protected val afterTickTriggers = mutable.Map[String, (Net)=>Any]()
  def addAfterTickTrigger(id: String, f: (Net) => Any):Unit = afterTickTriggers.contains(id) match {
    case false => afterTickTriggers.put(id, f)
    case true => throw new IllegalArgumentException(s"There was already registered an after tick trigger with id $id")
  } 
  def addAfterTickTrigger(f: (Net) => Any):Unit = addAfterTickTrigger("anon"+afterTickTriggers.size,f)
  def isAfterTickTrigger(id: String) = afterTickTriggers.contains(id)
  def removeAfterTickTrigger(id: String) = afterTickTriggers.remove(id)
  def clearAfterTickTriggers() = afterTickTriggers.clear
}