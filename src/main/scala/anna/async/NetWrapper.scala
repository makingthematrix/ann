package anna.async

import anna.Context
import anna.logger.LOG
import anna.utils.Utils._

import scala.collection.mutable

class NetWrapper(val net: NetRef) {
  lazy val ids = net.inputIds
  lazy val size = net.inputSize
  
  private val inputQueue = mutable.Queue[Seq[Double]]()
  
  private var _iteration = 0
  def iteration = _iteration 

  def resetIterations() = {
    _iteration = 0
  }

  def find(id: String) = net.find(id).neuronOpt match {
    case Some(nref) => nref
    case None => throw new IllegalArgumentException(s"There is no output neuron with id $id")
  }

  def info(id: String) = find(id).info
    
  def add(input: Seq[Double]) = {
	  assert(input.length == size, s"The input vector has to be exactly ${size} numbers long and is ${input.length}.")
    inputQueue += input
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
  )).foreach( add )

  def +=(c: Char) = {
    if(signRegister.contains(c)) add(Seq(signRegister(c)))
    else throw new IllegalArgumentException(s"No input registered with sign $c")
  }

  def deregSign(sign: Char) = signRegister -= sign

  def tick(c: Char):Unit = {
    this += c
    tick(1)
  }
  def tick():Unit = tick(1)
  def tick(n: Int):Unit = {
    for(i <- 1 to n) {
      LOG.debug(s"----- iteration ${_iteration} -----")
      val input = if(inputQueue.nonEmpty) inputQueue.dequeue else generateEmptyInput
      net.signal(input)
      Thread.sleep(Context().iterationTime)
      _iteration = _iteration + 1
    }
    LOG.debug(s"----- iteration ${_iteration} -----")
  }

  var timeout = 100

  def iterateUntilCalm(inputVector: String):Int = {
    this += inputVector
    iterateUntilCalm()
  }

  def iterateUntilCalm():Int = {
    var neuronFired = false
    net.addAfterFireToAll("tickUntilCalm")( (_:Double)=>{ neuronFired = true })

    var (calmTick, counter) = (0, 0)
    while(inputQueue.nonEmpty || (calmTick < 3 && counter < timeout)){
      neuronFired = false
      tick()
      if(neuronFired) calmTick = 0 else calmTick += 1
      counter += 1
    }
    
    net.reset()

    net.removeAfterFireFromAll("tickUntilCalm")
    counter
  }

  def addAfterFire(id: String, name: String)(f: => Any): Unit = net.addAfterFire(id, name)(f)
  def addAfterFire(id: String)(f: => Any): Unit = addAfterFire(id, id)(f)

  def addSilenceRequested(id: String, name: String)(f: => Any): Unit = net.addSilenceRequested(id, name)(f)
  def addSilenceRequested(id: String)(f: => Any):Unit  = addSilenceRequested(id, id)(f)

  def addSignalIgnored(id: String, name: String)(f: => Any): Unit = net.addSignalIgnored(id, name)(f)
  def addSignalIgnored(id: String)(f: => Any):Unit  = addSignalIgnored(id, id)(f)

  def shutdown() = {
    net.shutdown()
    Thread.sleep(100L)
  }

  def reset() = net.reset()
  def removeAllTriggers() = net.removeAllTriggers()

  def empty = inputQueue.isEmpty
}

object NetWrapper {
  def apply(net: NetRef) = {
    val ani = new NetWrapper(net)
    ani.regSign('0',0.0)
    ani.regSign('1', 1.0)
    ani
  }
}