package anna.sync

import scala.collection.mutable

trait AbstractNet[N <: Neuron] {
  protected def inputLayer: Seq[N] 
  protected def middleLayer: Seq[N]
  protected def outputLayer: Seq[N] 
  
  protected var iterationCounter = 0L
  
  def setInput(in: Seq[Double]){
    val ins = inputLayer
    assert(ins.size == in.size, s"Difference in size between the input layer (${ins.size}) and the input (${in.size})")
    
    ins.zip(in).foreach( tuple => tuple._1 += tuple._2 )
  }
  
  def output = outputLayer.map( _.lastOutput )
  
  def iteration = iterationCounter
  
  def size = inputLayer.size + middleLayer.size + outputLayer.size
  def inputSize = inputLayer.size
  def middleSize = middleLayer.size
  def outputSize = outputLayer.size
  
  def ids = inputIds ++ middleIds ++ outputIds
  def inputIds = inputLayer.map( _.getId )
  def middleIds = middleLayer.map( _.getId )
  def outputIds = outputLayer.map( _.getId )
  
  def find(id: String):Option[N] = {
    val inFind = inputLayer.find( _.getId == id )
    if(inFind.isDefined) return inFind
    val midFind = middleLayer.find( _.getId == id )
    if(midFind.isDefined) return midFind
    outputLayer.find( _.getId == id )
  }
  
  protected def find(id1: String, id2: String):(N,N) = {
    val n1 = find(id1)
    if(n1.isEmpty) throw new IllegalArgumentException("There is no neuron with id " + id1)
    val n2 = find(id2)
    if(n2.isEmpty) throw new IllegalArgumentException("There is no neuron with id " + id2)
    (n1.get,n2.get)
  }
  
  def contains(id: String) = find(id).isDefined
  
  protected val afterTickTriggers = mutable.Map[String,(AbstractNet[N])=>Any]()
  def addAfterTickTrigger(id: String, f: (AbstractNet[N]) => Any):Unit = afterTickTriggers.contains(id) match {
    case false => afterTickTriggers.put(id, f)
    case true => throw new IllegalArgumentException(s"There was already registered an after tick trigger with id $id")
  } 
  def addAfterTickTrigger(f: (AbstractNet[N]) => Any):Unit = addAfterTickTrigger("anon"+afterTickTriggers.size,f)
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