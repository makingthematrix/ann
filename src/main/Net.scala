package main

import scala.collection.mutable
import scala.concurrent._
import ExecutionContext.Implicits.global
import Utils._

class Net(val defSlope: Double = 20.0,val defTreshold: Double = 0.5, val defWeight: Double = 1.0) extends AbstractNet {
  private val neurons = mutable.ListBuffer[Neuron]()
  private val ins = mutable.ListBuffer[Neuron]()
  private val outs = mutable.ListBuffer[Neuron]()
  
  override protected def inputLayer: Seq[Neuron] = ins.toSeq
  override protected def outputLayer: Seq[Neuron] = outs.toSeq
  override protected def middleLayer: Seq[Neuron] = {
    val inputIds = ins.map( _.id ).toSet
    val outputIds = outs.map( _.id ).toSet
    neurons.filterNot( n => inputIds.contains(n.id) || outputIds.contains(n.id))
           .sortWith((n1,n2) => n1.id < n2.id)
  }
  
  override def ids = neurons.map( _.id )
  override def find(id: String) = neurons.find( _.id == id )
  
  def addNeuron(slope: Double =defSlope, treshold: Double =defTreshold): Neuron = {
    val n = Neuron(treshold,slope)
    neurons += n
    n
  }
  def addNeuron(neuron: Neuron) = neurons += neuron
  
  override def size = neurons.size
  
  def connect(id1: String, id2: String): Boolean = connect(id1, id2, defWeight)
  def connect(id1: String, id2: String, weight: Double): Boolean = {
    val n1 = neurons.find(_.id == id1)
    assert(n1 == None, s"Unable to find a neuron with id $id1")
    val n2 = neurons.find(_.id == id2)
    assert(n2 == None, s"Unable to find a neuron with id $id2")
    connect(n1.get, n2.get, weight)
  }
  def connect(n1: Neuron, n2: Neuron): Boolean = connect(n1, n2, defWeight)
  def connect(n1: Neuron, n2: Neuron, weight: Double): Boolean = n1.connect(n2, weight)
  
  def setInputLayer(inputIds: Seq[String]) = {
    val in = inputIds.map( id => neurons.find( _.id == id) match {
      case Some(n) => n
      case None => throw new IllegalArgumentException(s"Unable to find a neuron with id $id")
    }).sortWith((n1,n2) => n1.id < n2.id)
    clearInputLayer
    ins ++= in
  }
  
  def addToInputLayer(n: Neuron){
    neurons += n
    ins += n
  }
  
  def clearInputLayer = ins.clear
  
  override def inputSize = ins.size
 
  def setOutputLayer(outputIds: Seq[String]) = {
    val out = outputIds.map( id => neurons.find( _.id == id) match {
      case Some(n) => n
      case None => throw new IllegalArgumentException(s"Unable to find a neuron with id $id")
    }).sortWith((n1,n2) => n1.id < n2.id)
    clearOutputLayer
    outs ++= out
  }
  
  def clearOutputLayer = outs.clear
  
  override def outputSize = outs.size
  
  def addToOutputLayer(n: Neuron){
    neurons += n
    outs += n
  }

  private val tickWaiting = mutable.Queue[Neuron]()
  private var constantTick: Future[Unit] = null
  @volatile var constantTickWorks = false

  def addToWaiting(neuron: Neuron) = tickWaiting += neuron

  private def tickConstantly() = /*async */ future {
    while(constantTickWorks){
      if(tickWaiting.nonEmpty) tickWaiting.dequeue.tick()
      /* await */ Thread.sleep(100L)
    }
  }
  
  def start() = if(!constantTickWorks){
    constantTickWorks = true
    constantTick = tickConstantly()
  }

  def stop() = if(constantTickWorks){
    constantTickWorks = false
    constantTick = null
  }  
  
  def isWorking = constantTickWorks
}

object Net {
  def apply() = new Net()
  
  def apply(neuronNumber: Int) = {
    val net = new Net()
    for(i <- 1 to neuronNumber) net.addNeuron()
    net
  }
  
  def apply(slope: Double, treshold: Double, weight: Double, neuronNumber: Int) = {
    val net = new Net(slope, treshold)
    for(i <- 1 to neuronNumber) net.addNeuron()
    net
  }
  
  def apply(slope: Double, treshold: Double, weight: Double) = new Net(slope, treshold, weight)
  
  implicit def toString(seq: Seq[Double]): String = {
    val sb = StringBuilder.newBuilder
    seq.foreach(d => {
      if(sb.nonEmpty) sb.append(',')
      sb.append(d)
    })
    sb.toString
  }
}