package main

trait NeuronLike { 
  def getId: String
  def input:Double
  def lastOutput:Double
  
  def silence():Unit
  
  protected def calculateOutput: Double 
  def getSynapses: List[Synapse] 
  
  def tick():Unit
  protected def run()
  
  def +=(signal: Double):Unit
  
  def connect(destination:NeuronLike, weight: Double):Boolean
  def disconnect(destination: NeuronLike):Unit
  
  def findSynapse(destination: NeuronLike):Option[Synapse]
  def weightSum = getSynapses.map(_.weight).sum
  def absWeightSum = getSynapses.map( s => math.abs(s.weight) ).sum

  def averageWeight = weightSum / getSynapses.size

  def normalize = {
    val ws = weightSum
    getSynapses.foreach( _.weight /= ws )
  }

  def isPositive = !getSynapses.exists( _.weight < 0.0 )
  def isNegative = !getSynapses.exists( _.weight > 0.0 )
  def isMixed = getSynapses.exists( _.weight < 0.0 ) && getSynapses.exists( _.weight > 0.0 )
}