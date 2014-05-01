package main

class DelayNeuron(id: String, treshold: Double = 0.5, slope: Double = 20.0) extends Neuron(id, treshold, slope) {
  protected var lastTickBuffer = 0.0
  override def tick() = {
    println(s"--- $id tick with buffer $lastTickBuffer and treshold $treshold")
    if(lastTickBuffer > treshold) run()
    afterTickTriggers.values.foreach( _(this) )
    lastTickBuffer = buffer
    buffer = 0.0
  }
   
  override protected def run(){
    output = calculateOutput
    println(s"output $output")
    synapses foreach { _.send(output) } 
    afterFireTriggers.values.foreach( _(this) )
  } 
  
  override protected def calculateOutput = lastTickBuffer match {
    case x if x <= 0.0 => 0.0
    case x if x >= 1.0 => 1.0
    case x => 1.0/(1.0+Math.exp(-slope*(x-0.5)));
  }
}

object DelayNeuron{
  private var serialId = 1L
  
  def getSerialId = serialId

  def apply() = {
    val n = new DelayNeuron("delay_"+serialId)
    serialId += 1
    n
  }
  
  def apply(treshold: Double, slope: Double) = {
    val n = new DelayNeuron("delay_"+serialId, treshold, slope)
    serialId += 1
    n
  }
  
  def apply(id: Long, treshold: Double, slope: Double) = {
    val n = new DelayNeuron("delay_"+id, treshold, slope)
    if(serialId <= id) serialId = id + 1
    n
  }
  
  def apply(id: String, treshold: Double, slope: Double) = new DelayNeuron(id, treshold, slope)
}