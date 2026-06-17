package anna.async

import anna.Context
import anna.logger.LOG
import anna.utils.Utils._

import scala.collection.mutable

final class NetWrapper(val net: NetRef) {
  lazy val ids: Seq[String] = net.inputIds
  lazy val size: Int = net.inputSize

  private val signRegister = mutable.Map[Char,Double]()
  private val inputQueue = mutable.Queue[List[Double]]()
  private var _iteration = 1

  def iteration: Int = _iteration

  def resetIterations(): Unit = {
    _iteration = 1
  }

  def find(id: String): NeuronRef = net.find(id).neuronOpt match {
    case Some(nref) => nref
    case None => throw new IllegalArgumentException(s"There is no output neuron with id $id")
  }

  def info(id: String): Messages.NeuronInfo = find(id).info
    
  def add(input: List[Double]): Unit = {
	  assert(input.length == size, s"The input vector has to be exactly ${size} numbers long and is ${input.length}.")
    inputQueue += input
  }
  
  def addEmptyInput(): Unit = add(generateEmptyInput)
  def +=(input: List[Double]): Unit = add(input)
  def +=(d: Double): Unit = add(List(d))
  def +=(t: (Double,Double)): Unit = add(List(t._1,t._2))
  def +=(t: (Double,Double,Double)): Unit = add(List(t._1,t._2,t._3))
  
  private def generateEmptyInput: List[Double] = List.fill(size)(0.0)

  def regSign(sign: Char,input: Double): Unit = signRegister += (sign -> input)

  def +=(input: String): Unit = {
    val in = input.split(",").toList
    in.map( str => str.toCharArray.map(
      c => if(signRegister.contains(c)) signRegister(c)
      else throw new IllegalArgumentException(s"No input registered with sign $c")
    ).toList).foreach( add )
  }

  def +=(c: Char): Unit = {
    if(signRegister.contains(c)) add(List(signRegister(c)))
    else throw new IllegalArgumentException(s"No input registered with sign $c")
  }

  def deregSign(sign: Char): Unit = signRegister -= sign

  def tick(c: Char):Unit = {
    this += c
    tick(1)
  }

  private def popInput() = if(inputQueue.nonEmpty) inputQueue.dequeue else generateEmptyInput

  def tick():Unit = tick(1)

  def tick(n: Int):Unit = {
    for(i <- 1 to n) {
      val input = popInput()
      LOG.debug(s"input: $input")
      if(input.sum > 0.0) { // so if the input is empty we do nothing
        net.signal(input)
      }
      Thread.sleep(Context().iterationTime)
      _iteration = _iteration + 1
    }
  }

  def iterateUntilCalm(inputVector: String):Int = {
    this += inputVector
    iterateUntilCalm()
  }

  def iterateUntilCalm():Int = {
    var neuronFired = false
    net.addAfterFireToAll("tickUntilCalm"){ neuronFired = true }

    var (calmTick, counter) = (0, 0)
    val maxIterations = Context().maxRunIterations
    while(inputQueue.nonEmpty || (calmTick < 3 && counter < maxIterations)){
      neuronFired = false
      tick()
      if(neuronFired) calmTick = 0 else calmTick += 1
      counter += 1
    }

    net.removeAfterFireFromAll("tickUntilCalm")
    counter
  }

  def addAfterFire(id: String, name: String)(f: => Any): Unit = net.addAfterFire(id, name)(f)
  def addAfterFire(id: String)(f: => Any): Unit = addAfterFire(id, id)(f)

  def addSilenceRequested(id: String, name: String)(f: => Any): Unit = net.addSilenceRequested(id, name)(f)
  def addSilenceRequested(id: String)(f: => Any):Unit  = addSilenceRequested(id, id)(f)

  def addSignalIgnored(id: String, name: String)(f: => Any): Unit = net.addSignalIgnored(id, name)(f)
  def addSignalIgnored(id: String)(f: => Any):Unit  = addSignalIgnored(id, id)(f)

  def shutdown(): Unit = {
    net.shutdown()
    Thread.sleep(200L)
  }

  def reset(): Messages.Success = net.reset()
  def removeAllTriggers(): Messages.Success = net.removeAllTriggers()

  def empty: Boolean = inputQueue.isEmpty

  def neuronIds = net.neuronsIds
}

object NetWrapper {
  def apply(net: NetRef) = {
    val ani = new NetWrapper(net)
    ani.regSign('0',0.0)
    ani.regSign('1', 1.0)
    ani
  }
}