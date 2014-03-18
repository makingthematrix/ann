package main

import scala.collection.mutable

class NetInput(val net: Net) {
  private val inputQueue = mutable.Queue[Seq[Double]]()
  
  def add(input: Seq[Double]) = {
    if(input.length != net.inputSize)
      throw new IllegalArgumentException(s"The input vector has to be exactly ${net.inputSize} numbers long.")
    inputQueue += input
  }
  def addEmptyInput = add(generateEmptyInput)
  def +=(input: Seq[Double]) = add(input)
  def +=(d: Double) = add(Seq(d))
  def +=(t: (Double,Double)) = add(Seq(t._1,t._2))
  def +=(t: (Double,Double,Double)) = add(Seq(t._1,t._2,t._3))

  def size = inputQueue.length

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
}