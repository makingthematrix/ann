package anna.async

import anna.async.NetBuilderOps._
import anna.utils.DoubleRange
import anna.utils.DoubleRange._
import org.junit.Assert._
import org.junit.Test

import scala.annotation.tailrec


class NeuronSuite extends MySuite {
  case class NeuronData(weight: Double, threshold: Double)

  def oneIteration(input: Double, weight: Double) = weight
  
  @tailrec
  final def countIterations(input: Double, data: NeuronData, currentIteration: Int =0)(implicit timeout: Int):Int =
    if(currentIteration == timeout) Int.MaxValue
    else if(input >= data.threshold) currentIteration
    else countIterations(oneIteration(input, data.weight), data, currentIteration + 1)
  
  def neuronDataIter(weightRange: DoubleRange, thresholdRange: DoubleRange, resolution: Int) =
      for{ w <- weightRange.iterator(resolution)
           t <- thresholdRange.iterator(resolution) } yield NeuronData(w, t)
    
  @Test def findLongestLoop() = {
    implicit val timeout = 10
    val iter = neuronDataIter(0.8<=>1.0, 0.5<=>0.9, 20)
    val input = 0.55
    
    var maxIterations = 0
    var foundData: NeuronData = null
    for(data <- iter){
      val result = countIterations(input, data)
      if(result > maxIterations && result < Int.MaxValue && result > 0){
        maxIterations = result
        foundData = data
      }
    }
    
    println(s"maxIterations: $maxIterations, data: $foundData")
  }
            
  @Test def delayedSignal() = {
    builder.addInput("in")
           .chainDummy("mi11", 0.55)
           .loop("loop",1.0,0.0,0.99)
           .chain("out",1.0,0.66)
    builder.use("mi11").hush("in")
    builder.use("out").connect("mi11",-1.0).connect("loop",-1.0)
    build()
    
    var (goodLoops, badLoops) = (0, 0)
    var done = false
    netWrapper.addAfterFire("in")( (_:Double)=>{ println("INCOMING!") } )
    netWrapper.addAfterFire("loop")( (_:Double)=>{
      println("here") 
      if(!done) goodLoops += 1 else badLoops += 1
    })
    netWrapper.addAfterFire("out")( (_:Double)=>{
      println("DONE") 
      done = true
    })
    netWrapper += "1"
    init()
    val interval = netWrapper.tickUntilCalm()
    println(s"interval: $interval, good loops: $goodLoops, bad loops: $badLoops")
    assertEquals(0, badLoops)
  }
            
}