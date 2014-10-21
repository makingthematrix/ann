package main.sync

import scala.collection.mutable

class NetInput(val name: String, val net: Net, val resolution: Int = 1) {
  def ids = net.inputIds
  def size = net.inputSize
  
  private val inputQueue = mutable.Queue[Seq[Double]]()
  
  def find(id: String) = net.inputIds.contains(id) match {
    case true => net.find(id).get
    case false => throw new IllegalArgumentException(s"There is no output neuron with id $id")
  }
  
  def add(input: Seq[Double]) = {
	assert(input.length == size, s"The input vector has to be exactly ${size} numbers long and is ${input.length}.")
    for(counter <- 1 to resolution) {
      val inputBuffer = mutable.ListBuffer[Double]()
      inputBuffer ++= input
      inputQueue += inputBuffer.toSeq
    }
  }
  
  def addEmptyInput = add(generateEmptyInput)
  def +=(input: Seq[Double]) = add(input)
  def +=(d: Double) = add(Seq(d))
  def +=(t: (Double,Double)) = add(Seq(t._1,t._2))
  def +=(t: (Double,Double,Double)) = add(Seq(t._1,t._2,t._3))

  def inputQueueLength = inputQueue.length

  def tick():Unit = tick(1)
  def tick(n: Int):Unit = for(i <- 1 to n * resolution){
    val input = if(inputQueue.nonEmpty) inputQueue.dequeue else generateEmptyInput
    net.setInput(input)
    net.tick()
  }
  
  def tickUntilCalm(timeout: Int = 100) = {
    var neuronFired = false
    net.getNeurons.foreach(_.addAfterFireTrigger("tickUntilCalm", () => neuronFired = true))
    
    var calmTick = 0
    var counter = 0
    while(calmTick < 3 && counter < timeout){
      neuronFired = false
      tick()
      if(neuronFired) calmTick = 0
      else calmTick += 1
      counter += 1
    }
    
    net.getNeurons.foreach(_.removeAfterFireTrigger("tickUntilCalm"))
    counter
  }

  def tickEmptyInput():Unit = tickEmptyInput(1)
  def tickEmptyInput(n: Int):Unit = for(i <- 1 to n * resolution){
    net.setInput(generateEmptyInput)
    net.tick()
  }

  def generateEmptyInput:Seq[Double] = for(i <- 1 to net.inputSize) yield 0.0
  
  private val signRegister = mutable.Map[Char,Double]()
  
  def regSign(sign: Char,input: Double) = signRegister += (sign -> input)
  def +=(input: String) = input.split(",").toSeq.map( 
    _.toCharArray().toSeq.map( c => 
      if(signRegister.contains(c)) signRegister(c) 
      else throw new IllegalArgumentException(s"No input registered with sign $c")
  )).foreach( add(_) )
  
}

object NetInput {
  def apply(name: String, net: Net, resolution: Int = 1) = {
    val ni = new NetInput(name, net, resolution)
    ni.regSign('0',0.0)
    ni.regSign('1', 1.0)
    ni
  }
}