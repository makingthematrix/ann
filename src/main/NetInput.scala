package main

import scala.collection.mutable
import Utils._

class NetInput(val name: String, val net: Net, val resolution: Int = 1) {
  def ids = net.inputIds
  def size = net.inputSize
  
  private val inputQueue = mutable.Queue[Seq[Double]]()
  
  def find(id: String) = net.inputIds.contains(id) match {
    case true => net.find(id).get
    case false => throw new IllegalArgumentException(s"There is no output neuron with id $id")
  }
  
  def add(input: Seq[Double]) = {
    assert(input.length != net.inputSize, s"The input vector has to be exactly ${net.inputSize} numbers long.")
    val inputBuffer = mutable.ListBuffer[Double]()
    for(counter <- 1 to resolution) inputBuffer ++= input
    inputQueue += inputBuffer.toSeq
  }
  
  def addEmptyInput = add(generateEmptyInput)
  def +=(input: Seq[Double]) = add(input)
  def +=(d: Double) = add(Seq(d))
  def +=(t: (Double,Double)) = add(Seq(t._1,t._2))
  def +=(t: (Double,Double,Double)) = add(Seq(t._1,t._2,t._3))

  def inputQueueLength = inputQueue.length

  def tick():Unit = tick(1)
  def tick(n: Int):Unit = for(i <- 1 to n){
    val input = if(inputQueue.nonEmpty) inputQueue.dequeue else generateEmptyInput
    net.setInput(input)
    net.tick()
  }

  def tickEmptyInput():Unit = tickEmptyInput(1)
  def tickEmptyInput(n: Int):Unit = for(i <- 1 to n){
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