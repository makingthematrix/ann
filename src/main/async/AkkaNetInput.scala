package main.async

import scala.collection.mutable

class AkkaNetInput(val name: String, val net: NetRef, val resolution: Int = 1) {
  lazy val ids = net.inputIds
  lazy val size = net.inputSize
  
  private val inputQueue = mutable.Queue[Seq[Double]]()
  
  def find(id: String) = ids.contains(id) match {
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
  
  def generateEmptyInput:Seq[Double] = for(i <- 1 to size) yield 0.0
  
  private val signRegister = mutable.Map[Char,Double]()
  
  def regSign(sign: Char,input: Double) = signRegister += (sign -> input)
  def +=(input: String) = input.split(",").toSeq.map( 
    _.toCharArray().toSeq.map( c => 
      if(signRegister.contains(c)) signRegister(c) 
      else throw new IllegalArgumentException(s"No input registered with sign $c")
  )).foreach( add(_) )

}

object AkkaNetInput {
  def apply(name: String, net: NetRef, resolution: Int = 1) = new AkkaNetInput(name, net: NetRef, resolution)
}