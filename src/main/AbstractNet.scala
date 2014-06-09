package main

import scala.collection.mutable

trait AbstractNet {
  protected def inputLayer: Seq[Neuron] 
  protected def middleLayer: Seq[Neuron]
  protected def outputLayer: Seq[Neuron] 
  
  private var iterationCounter = 0L
  
  def setInput(in: Seq[Double]){
    val ins = inputLayer
    if(ins.size != in.size)
      throw new IllegalArgumentException(s"Difference in size between the input layer (${ins.size}) and the input (${in.size})")
    
    ins.zip(in).foreach( tuple => tuple._1 += tuple._2 )
  }
  
  def output = outputLayer.map( _.lastOutput )
  
  def tick(){
    // this is a synchronous tick of all neurons - first the input layer, then the middle, then the output layer
    // not really what we want to achieve here ;)
	iterationCounter += 1
    println(s"--- tick nr $iterationCounter ---")
    inputLayer.foreach( _.tick() )
    middleLayer.foreach( _.tick() )
    outputLayer.foreach( _.tick() )
    afterTickTriggers.values.foreach( _(this) )
  }
  
  def iteration = iterationCounter
  
  def size = inputLayer.size + middleLayer.size + outputLayer.size
  def inputSize = inputLayer.size
  def middleSize = middleLayer.size
  def outputSize = outputLayer.size
  
  def ids = inputIds ++ middleIds ++ outputIds
  def inputIds = inputLayer.map( _.id )
  def middleIds = middleLayer.map( _.id )
  def outputIds = outputLayer.map( _.id )
  
  def find(id: String):Option[Neuron] = {
    val inFind = inputLayer.find( _.id == id )
    if(inFind.isDefined) return inFind
    val midFind = middleLayer.find( _.id == id )
    if(midFind.isDefined) return midFind
    outputLayer.find( _.id == id )
  }
  
  protected def find(id1: String, id2: String):(Neuron,Neuron) = {
    val n1 = find(id1)
    if(n1.isEmpty) throw new IllegalArgumentException("There is no neuron with id " + id1)
    val n2 = find(id2)
    if(n2.isEmpty) throw new IllegalArgumentException("There is no neuron with id " + id2)
    (n1.get,n2.get)
  }
  
  def contains(id: String) = find(id).isDefined
  
  protected val afterTickTriggers = mutable.Map[String,(AbstractNet)=>Any]()
  def addAfterTickTrigger(id: String, f: (AbstractNet) => Any):Unit = afterTickTriggers.contains(id) match {
    case false => afterTickTriggers.put(id, f)
    case true => throw new IllegalArgumentException(s"There was already registered an after tick trigger with id $id")
  } 
  def addAfterTickTrigger(f: (AbstractNet) => Any):Unit = addAfterTickTrigger("anon"+afterTickTriggers.size,f)
  def isAfterTickTrigger(id: String) = afterTickTriggers.contains(id)
  def removeAfterTickTrigger(id: String) = afterTickTriggers.remove(id)
  def clearAfterTickTriggers() = afterTickTriggers.clear

  def countSynapses = 
    inputLayer.flatMap( _.getSynapses ).length +
    middleLayer.flatMap( _.getSynapses ).length +
    outputLayer.flatMap( _.getSynapses ).length
    
  def outputSum = 
    inputLayer.map( _.lastOutput ).sum +
    middleLayer.map( _.lastOutput ).sum +
    outputLayer.map( _.lastOutput ).sum
    
  def weightSum = 
    inputLayer.map( _.weightSum ).sum +
    middleLayer.map( _.weightSum ).sum +
    outputLayer.map( _.weightSum ).sum  
 
  def absWeightSum = 
    inputLayer.map( _.absWeightSum ).sum +
    middleLayer.map( _.absWeightSum ).sum +
    outputLayer.map( _.absWeightSum ).sum  
 
}