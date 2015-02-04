package anna.epengine

import anna.async.NeuronType
import anna.data._
import anna.epengine.TossType._
import anna.utils.DoubleRange
import anna.utils.DoubleRange._
import anna.utils.IntRange
import anna.logger.LOG._

import scala.util.Random

object MutationAccess extends Enumeration {
  type MutationAccess = Value
  val FULL, DONTDELETE, NONE = Value
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

  var neuronsRange:Range = 5 to 10
  var inputIds = List[String]()
  var outputIds = List[String]()
  var synapsesDensity:Double = 2.5
  var inputTickMultiplierRange:DoubleRange = 1.0<=>3.0
  
  private def synapseTraitToss() = {
    val weightProbability: Probability = 1.0 - synapseHushProbability
    val toss = RandomNumber()
    if(weightProbability > 0.0  && toss <= weightProbability) SynapseWeight(synapseWeightRange.choose(RandomNumber()))
    else Hush
  }


  def tossForSynapse(id: String) = {
    val weight = synapseTraitToss()
    SynapseChromosome(id, weight)
  }

  private def forgetTraitToss() = {
    val forgetValueProbability: Probability = 1.0 - dontForgetProbability - forgetAllProbability
    assert((0.0<=>1.0).contains(forgetValueProbability),s"The forget value probability does not add up. dontForgetProb=${dontForgetProbability}, forgetAllProb=${forgetAllProbability}")
    val toss = RandomNumber()
    if(toss < dontForgetProbability) DontForget
    else if(toss <= (dontForgetProbability + forgetValueProbability)) ForgetValue(forgettingRange.choose(RandomNumber()))
    else ForgetAll
  }

  implicit private def fromRange(r: Range):IntRange = IntRange(r)


  def tossForNeuron(id: String, accessMap: Map[String, MutationAccess.Value] = Map()) = {
    val threshold = thresholdRange.choose(RandomNumber())
    val slope = slopeRange.choose(RandomNumber())
    val hushValue = HushValue(hushRange.choose(RandomNumber()))
    val forgetting = forgetTraitToss()
    val tickTimeMultiplier = tickTimeMultiplierRange.choose(RandomNumber())

    NeuronChromosome(
      NeuronData(id, threshold, slope, hushValue, forgetting, Nil, tickTimeMultiplier, NeuronType.STANDARD),
      accessMap
    )
  }

  private def connect(from: NeuronChromosome, to:NeuronChromosome): Boolean = if(from.isConnectedTo(to.id)) false else {
    val synapseChromosome = tossForSynapse(to.id)
    from.addSynapse(synapseChromosome)
    true
  }

  private def chooseNeuron(neurons: List[NeuronChromosome], check:(NeuronChromosome)=>Boolean):Option[NeuronChromosome] = neurons match {
    case Nil => None
    case list => val index = (0 until list.size).choose(RandomNumber())
                 val n = list(index)
                 if(check(n)) Some(n) else chooseNeuron(list.filter(_.id != n.id), check)
  }

  def tossForNet(id: String) = {
    assert(synapsesDensity >= 1.0, "There should be at least one synapse for neuron")
    assert(inputIds.size + outputIds.size <= neuronsRange.end, s"You chose ${inputIds.size} inputs and ${outputIds.size} outputs, but the max possible neurons number is only ${neuronsRange.end}")
    val r = if(inputIds.size + outputIds.size > neuronsRange.start) (inputIds.size + outputIds.size) to neuronsRange.end else neuronsRange
    val neuronsSize = r.choose(RandomNumber())

    val ins = inputIds.map( tossForNeuron(_) )
    val outs = outputIds.map( tossForNeuron(_) )
    val middles = (for(i <- 1 to neuronsSize - ins.size - outs.size) yield( tossForNeuron(id+"_"+i) )).toList
    val ns = ins ++ middles ++ outs

    // at least one synapse from each "in" to one of "middles"
    var synapsesCounter = 0
    ins.foreach( in => {
      val check:(NeuronChromosome)=>Boolean = (n: NeuronChromosome) => { !in.isConnectedTo(n.id) }
      val middleOpt = chooseNeuron(middles, check)
      if(middleOpt != None) {
        connect(in, middleOpt.get)
        synapsesCounter += 1
      }
    })

    // at least one synapse to each "out" from one of "middles"
    outs.foreach( out => {
      val check:(NeuronChromosome)=>Boolean = (n: NeuronChromosome) => { !n.isConnectedTo(out.id) }
      val middleOpt = chooseNeuron(middles, check)
      if(middleOpt != None) {
        connect(middleOpt.get, out)
        synapsesCounter += 1
      }
    })

    val synapsesSize = Math.round(synapsesDensity * neuronsSize).toInt - synapsesCounter

    debug(this,"synapsesSize: " + synapsesSize)
    if(synapsesSize > 0) {
      val im = ins ++ middles
      debug(this, "im: " + im.size)
      val mo = middles ++ outs
      debug(this, "mo: " + mo.size)

      var i = 0
      while(i < synapsesSize) {
        val imIndex = (0 until im.size).choose(RandomNumber())
        val imNeuronChromosome = im(imIndex)
        val moIndex = (0 until mo.size).choose(RandomNumber())
        val moNeuronChromosome = mo(moIndex)
        connect(imNeuronChromosome, moNeuronChromosome)
        i += 1
      }
    }
    // @todo: it still doesn't ensure that there is a valid connection from ins to outs

    val inputTickMultiplier = inputTickMultiplierRange.choose(RandomNumber())
    NetChromosome(id, ns.map(_.data), inputIds, inputTickMultiplier)
  }
}

object Engine {
  private var instance:Option[Engine] = None

  def apply():Engine = instance match {
    case None => val engine = new Engine(); instance = Some(engine); engine
    case Some(engine) => engine
  }
}