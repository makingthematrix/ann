package test.async

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import main.utils.Utils._
import scala.annotation.tailrec

class NeuronSuite extends JUnitSuite {
  case class NeuronData(slope: Double, weight: Double, threshold: Double)
  case class DoubleRange(from: Double, to: Double)
  
  implicit class RichD(d: Double) {
    def <=>(other: Double) = DoubleRange(d, other)
  }
  
  def iterator(range: DoubleRange, resolution: Int) = 
    (0 to resolution).map{ x => (x.toDouble/resolution)*(range.to-range.from)+range.from }.view.iterator
  
  def oneIteration(input: Double, slope: Double, weight: Double) = f(input, slope) * weight
  
  @tailrec
  final def countIterations(input: Double, data: NeuronData, currentIteration: Int =0)(implicit timeout: Int):Int =
    if(currentIteration == timeout) Int.MaxValue
    else if(input >= data.threshold) currentIteration
    else countIterations(oneIteration(input, data.slope, data.weight), data, currentIteration + 1)
  
  def neuronDataIter(slopeRange: DoubleRange, weightRange: DoubleRange, thresholdRange: DoubleRange, resolution: Int) = 
      for { s <- iterator(slopeRange, resolution)
            w <- iterator(weightRange, resolution)
            t <- iterator(thresholdRange, resolution) } yield NeuronData(s, w, t)
    
  @Test def findLongestLoop() = {
    implicit val timeout = 10
    val iter = neuronDataIter(5.0<=>10.0, 0.8<=>1.0, 0.5<=>0.9, 20)
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
}