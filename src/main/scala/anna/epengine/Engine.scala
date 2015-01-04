package anna.epengine

import anna.async.NeuronType
import anna.data._
import anna.epengine.TossType._
import anna.utils.DoubleRange
import anna.utils.DoubleRange._

import scala.util.Random

case class IntRange(r: Range) extends AnyVal {
  def choose(x: Double):Int = math.round(x *(r.end - r.start) + r.start).toInt
}

class Engine {
  var synapseWeightRange: DoubleRange = 0.0<=>1.0
  var synapseHushProbability: Probability = 0.1
  var synapsesTossType: TossType = LINEAR
  var thresholdRange: DoubleRange = 0.0<=>0.9
  var slopeRange: DoubleRange = 1.0<=>20.0
  var hushRange:Range = 1 to 5
  var forgettingRange: DoubleRange = 0.1<=>0.9
  var dontForgetProbability: Probability = 0.75
  var forgetAllProbability: Probability = 0.05
  var tickTimeMultiplierRange: DoubleRange = 0.5<=>2.0

  private var _rand: Option[Random] = None
  private def rand = _rand match {
    case None =>
      val r = new Random()
      _rand = Some(r)
      r
    case Some(r) => r
  }
  
  private def synapseTraitToss() = {
    val weightProbability: Probability = 1.0 - synapseHushProbability
    val toss = rand.nextDouble()
    if(weightProbability > 0.0  && toss <= weightProbability) SynapseWeight(synapseWeightRange.choose(rand.nextDouble()))
    else Hush
  }

  private def forgetTraitToss() = {
    val forgetValueProbability: Probability = 1.0 - dontForgetProbability - forgetAllProbability
    assert((0.0<=>1.0).contains(forgetValueProbability),s"The forget value probability does not add up. dontForgetProb=${dontForgetProbability}, forgetAllProb=${forgetAllProbability}")
    val toss = rand.nextDouble()
    if(toss < dontForgetProbability) DontForget
    else if(toss <= (dontForgetProbability + forgetValueProbability)) ForgetValue(forgettingRange.choose(rand.nextDouble()))
    else ForgetAll
  }

  implicit private def fromRange(r: Range):IntRange = IntRange(r)
  
  def tossForSynapse(id: String) = {
    val weight = synapseTraitToss()
    SynapseChromosome(id, weight)
  }

  def tossForNeuron(id: String) = {
    val threshold = thresholdRange.choose(rand.nextDouble())
    val slope = slopeRange.choose(rand.nextDouble())
    val hushValue = HushValue(hushRange.choose(rand.nextDouble()))
    val forgetting = forgetTraitToss()
    val tickTimeMultiplier = tickTimeMultiplierRange.choose(rand.nextDouble())

    NeuronChromosome(id, threshold, slope, hushValue, forgetting, Nil, tickTimeMultiplier, NeuronType.STANDARD)
  }
}