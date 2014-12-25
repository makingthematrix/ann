package anna.async

import akka.actor._
import anna.async.Messages._
import anna.async.logger.LOG._
import anna.data.{ForgetTrait, HushValue}

import scala.collection.mutable

class Net(val id: String) extends Actor {
  private val neurons = mutable.ListBuffer[NeuronRef]()
  private val ins = mutable.ListBuffer[NeuronRef]()
  
  def receive: Receive = {
    case GetId => sender ! Msg(0.0, id)
    case GetNeurons => sender ! MsgNeurons(neurons.toList)
    case CreateNeuron(id, threshold, slope, hushValue, forgetting) => createNeuron(id, threshold, slope, hushValue, forgetting)
    case CreateDummy(id, hushValue) => createDummy(id, hushValue)
    case CreateHushNeuron(id) => createHushNeuron(id)
    case Shutdown => shutdown()
    case GetInputLayer => sender ! MsgNeurons(inputLayer.toList)
    case SetInputLayer(ids) => setInputLayer(ids)
    case GetMiddleLayer => sender ! MsgNeurons(middleLayer.toList)
    case GetNeuron(id) => sender ! MsgNeuron(findRef(id))
    case SignalSeq(in) => signal(in)
  }
  
  def shutdowning(caller: ActorRef): Receive = {
    case NeuronShutdownDone(id) =>
      remove(id)
      if(neurons.isEmpty){
        caller ! NetShutdownDone(id) 
        context.stop(self)
      }
  }
  
  private def inputLayer = ins.toSeq
  private def middleLayer = {
    val inputIds = ins.map( _.id ).toSet
    neurons.filterNot( n => inputIds.contains(n.id) )
  }

  private def remove(id: String) = findRef(id) match {
    case Some(ref) => neurons -= ref
    case None => 
  }
  
  
  private def shutdown() = {
    debug(s"shutdown for $id")
    context.become( shutdowning(sender) )
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
  
  private def createHushNeuron(id: String){
    val ref = context.actorOf(Props(new HushNeuron(id)), name=id)
    add(id, ref)
  }
  
  private def signal(in: Seq[Double]){
    assert(in.size == ins.size, s"Difference in size between the input layer (${ins.size}) and the input (${in.size})")
    debug(this, s"signal received in $id: " + in.mkString(", "))
    ins.zip(in).foreach( tuple => tuple._1 += tuple._2 )
  }
  
  private def setInputLayer(ids: Seq[String]){
    debug(Net.this, s"input layer set in $id: " + ids.mkString(", "))
    ins.clear()
    neurons.filter( n => ids.contains(n.id) ).foreach( ins += _ )
    sender ! Success("setInputLayer_"+id)
  }
  
  private def findRef(id: String):Option[NeuronRef] = neurons.find(_.id == id)

}