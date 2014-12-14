package main.async.epengine

import main.utils.DoubleRange
import main.utils.DoubleRange._
import main.utils.Utils.assert
import main.async.SynapseTrait
import TossType._
import scala.util.Random
import main.async.Hush
import main.async.SynapseWeight

case class SynapseChromosome(id: String, weight: SynapseTrait)

class Engine {
  var synapseWeightRange: DoubleRange = 0.0<=>1.0
  var synapseHushProbability: Probability = 0.1
  var synapsesTossType: TossType = LINEAR
  
  private var _rand: Option[Random] = None
  private def rand = _rand match {
    case None =>
      val r = new Random()
      _rand = Some(r)
      r
    case Some(r) => r
  }
  
  private def synapseToss() = synapsesTossType match {
    case LINEAR => rand.nextDouble()
    case GAUSSIAN => rand.nextDouble() // @todo not implemented yet  
  }
  
  def tossForSynapse(id: String) = {
    val weightProbability: Probability = 1.0 - synapseHushProbability
    
    val toss = synapseToss()
    val weight = if(weightProbability > 0.0  && toss <= weightProbability){
      SynapseWeight(synapseWeightRange.choose(toss / weightProbability))
    } else Hush
    
    SynapseChromosome(id, weight)
  }
}