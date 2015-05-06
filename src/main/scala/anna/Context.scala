package anna

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.util.Timeout
import anna.data._
import anna.epengine.Probability
import anna.utils.DoubleRange._
import anna.utils.Utils.formats
import anna.utils.{DoubleRange, IntRange}
import com.typesafe.config.ConfigFactory
import org.json4s.native.Serialization.{read, writePretty}

import scala.concurrent.duration._

case class NeuronDefaults(
  slope: Double,
  threshold: Double,
  weight: SynapseTrait,
  hushValue: HushValue,
  forgetting: ForgetTrait,
  tickTime: Long
){
  def toJson = writePretty(this)
}

case class SynapseGenomeDefaults(
  weightRange: DoubleRange,
  hushProbability: Probability,
  fullWeightProbability: Probability
){
  def toJson = writePretty(this)
}

case class NeuronGenomeDefaults(
  thresholdRange: DoubleRange,
  slopeRange: DoubleRange,
  hushRange: IntRange,
  forgettingRange: DoubleRange,
  tickTimeMultiplierRange: DoubleRange,

  dontForgetProbability: Probability,
  forgetAllProbability: Probability,
  thresholdProbability: Probability,
  slopeProbability: Probability,
  forgettingProbability: Probability,
  hushValueProbability: Probability,
  tickTimeMultiplierProbability: Probability,

  synapseChangeProbability: Probability,
  addSynapseProbability: Probability,
  deleteSynapseProbability: Probability
){
  def toJson = writePretty(this)
}

case class NetGenomeDefaults(
  addNeuronProbability: Probability,
  deleteNeuronProbability: Probability,
  mutateNeuronProbability: Probability,
  inputTickMultiplierProbability: Probability,
  inputTickMultiplierRange: DoubleRange,
  neuronsRange: IntRange,
  synapsesDensity: Double
){
  def toJson = writePretty(this)
}

case class Context(
  awaitTimeout: Long,
  initialMutationsNumber: Int,
  genomePollSize: Int,
  exercisesSetDir: String,
  mutationProbability: Double,
  evolutionDir: String,

  neuronDefaults: NeuronDefaults,
  synapseGenomeDefaults: SynapseGenomeDefaults,
  neuronGenomeDefaults: NeuronGenomeDefaults,
  netGenomeDefaults: NetGenomeDefaults
){
  def timeout = Timeout(FiniteDuration.apply(awaitTimeout, TimeUnit.SECONDS))

  def system:ActorSystem = {
    implicit lazy val t = timeout
    lazy val s = ActorSystem("system")
    val p = () => s
    p()
  }

  def toJson = writePretty(this)

  def slope = neuronDefaults.slope
  def threshold = neuronDefaults.threshold
  def weight = neuronDefaults.weight
  def hushValue = neuronDefaults.hushValue
  def forgetting = neuronDefaults.forgetting
  def tickTime = neuronDefaults.tickTime

  def weightRange = synapseGenomeDefaults.weightRange
  def hushProbability = synapseGenomeDefaults.hushProbability
  def fullWeightProbability = synapseGenomeDefaults.fullWeightProbability

  def thresholdRange = neuronGenomeDefaults.thresholdRange
  def slopeRange = neuronGenomeDefaults.slopeRange
  def hushRange = neuronGenomeDefaults.hushRange
  def forgettingRange = neuronGenomeDefaults.forgettingRange
  def tickTimeMultiplierRange = neuronGenomeDefaults.tickTimeMultiplierRange

  def dontForgetProbability = neuronGenomeDefaults.dontForgetProbability
  def forgetAllProbability = neuronGenomeDefaults.forgetAllProbability
  def thresholdProbability = neuronGenomeDefaults.thresholdProbability
  def slopeProbability = neuronGenomeDefaults.slopeProbability
  def forgettingProbability = neuronGenomeDefaults.forgettingProbability
  def hushValueProbability = neuronGenomeDefaults.hushValueProbability
  def tickTimeMultiplierProbability = neuronGenomeDefaults.tickTimeMultiplierProbability

  def synapseChangeProbability = neuronGenomeDefaults.synapseChangeProbability
  def addSynapseProbability = neuronGenomeDefaults.addSynapseProbability
  def deleteSynapseProbability = neuronGenomeDefaults.deleteSynapseProbability

  def addNeuronProbability = netGenomeDefaults.addNeuronProbability
  def deleteNeuronProbability = netGenomeDefaults.deleteNeuronProbability
  def mutateNeuronProbability = netGenomeDefaults.mutateNeuronProbability
  def inputTickMultiplierProbability = netGenomeDefaults.inputTickMultiplierProbability
  def inputTickMultiplierRange = netGenomeDefaults.inputTickMultiplierRange

  def neuronsRange = netGenomeDefaults.neuronsRange
  def synapsesDensity = netGenomeDefaults.synapsesDensity
}

object Context {
  private var instance:Option[Context] = None

  def apply(): Context = {
    if(instance == None) init()
    instance.get
  }

  def set(newInstance: Context): Unit ={
    instance = Some(newInstance)
  }

  def reset(): Unit ={
    instance = None
  }

  private def that = instance.get

  def withWeightRange(weightRange: DoubleRange) =
    set(apply().copy(synapseGenomeDefaults = that.synapseGenomeDefaults.copy(weightRange = weightRange)))
  def withHushProbability(hushProbability: Probability) =
    set(apply().copy(synapseGenomeDefaults = that.synapseGenomeDefaults.copy(hushProbability = hushProbability)))
  def withFullWeightProbability(fullWeightProbability: Probability) =
    set(apply().copy(synapseGenomeDefaults = that.synapseGenomeDefaults.copy(fullWeightProbability = fullWeightProbability)))

  def withThresholdRange(thresholdRange: DoubleRange) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(thresholdRange = thresholdRange)))
  def withSlopeRange(slopeRange: DoubleRange) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(slopeRange = slopeRange)))
  def withHushRange(hushRange: IntRange) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(hushRange = hushRange)))
  def withForgettingRange(forgettingRange: DoubleRange) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(forgettingRange = forgettingRange)))
  def withTickTimeMultiplierRange(tickTimeMultiplierRange: DoubleRange) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(tickTimeMultiplierRange = tickTimeMultiplierRange)))

  def withDontForgetProbability(dontForgetProbability: Probability) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(dontForgetProbability = dontForgetProbability)))
  def withForgetAllProbability(forgetAllProbability: Probability) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(forgetAllProbability = forgetAllProbability)))
  def withThresholdProbability(thresholdProbability: Probability) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(thresholdProbability = thresholdProbability)))
  def withSlopeProbability(slopeProbability: Probability) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(slopeProbability = slopeProbability)))
  def withForgettingProbability(forgettingProbability: Probability) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(forgettingProbability = forgettingProbability)))
  def withHushValueProbability(hushValueProbability: Probability) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(hushValueProbability = hushValueProbability)))
  def withTickTimeMultiplierProbability(tickTimeMultiplierProbability: Probability) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(tickTimeMultiplierProbability = tickTimeMultiplierProbability)))

  def withSynapseChangeProbability(synapseChangeProbability: Probability) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(synapseChangeProbability = synapseChangeProbability)))
  def withAddSynapseProbability(addSynapseProbability: Probability) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(addSynapseProbability = addSynapseProbability)))
  def withDeleteSynapseProbability(deleteSynapseProbability: Probability) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(deleteSynapseProbability = deleteSynapseProbability)))

  def withAddNeuronProbability(addNeuronProbability: Probability) =
    set(apply().copy(netGenomeDefaults = that.netGenomeDefaults.copy(addNeuronProbability = addNeuronProbability)))
  def withDeleteNeuronProbability(deleteNeuronProbability: Probability) =
    set(apply().copy(netGenomeDefaults = that.netGenomeDefaults.copy(deleteNeuronProbability = deleteNeuronProbability)))
  def withMutateNeuronProbability(mutateNeuronProbability: Probability) =
    set(apply().copy(netGenomeDefaults = that.netGenomeDefaults.copy(mutateNeuronProbability = mutateNeuronProbability)))
  def withInputTickMultiplierProbability(inputTickMultiplierProbability: Probability) =
    set(apply().copy(netGenomeDefaults = that.netGenomeDefaults.copy(inputTickMultiplierProbability = inputTickMultiplierProbability)))
  def withInputTickMultiplierRange(inputTickMultiplierRange: DoubleRange) =
    set(apply().copy(netGenomeDefaults = that.netGenomeDefaults.copy(inputTickMultiplierRange = inputTickMultiplierRange)))

  def withNeuronsRange(neuronsRange: IntRange) =
    set(apply().copy(netGenomeDefaults = that.netGenomeDefaults.copy(neuronsRange = neuronsRange)))
  def withSynapsesDensity(synapsesDensity: Double) =
    set(apply().copy(netGenomeDefaults = that.netGenomeDefaults.copy(synapsesDensity = synapsesDensity)))

  def withSlope(slope: Double) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(slope = slope)))
  def withThreshold(threshold: Double) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(threshold = threshold)))
  def withWeight(weight: SynapseTrait) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(weight = weight)))
  def withHushValue(hushValue: HushValue) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(hushValue = hushValue)))
  def withForgetting(forgetting: ForgetTrait) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(forgetting = forgetting)))
  def withTickTime(tickTime: Long) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(tickTime = tickTime)))

  private def init(): Unit ={
    val config = ConfigFactory.load()
    val root = config.getConfig("context")

    val awaitTimeout = root.getInt("awaitTimeout")
    //val system = ActorSystem("system")

    val exercisesSetDir = root.getString("exercisesSetDir")
    val genomePollSize = root.getInt("genomePollSize")
    val initialMutationsNumber = root.getInt("initialMutationsNumber")
    val mutationProbability = root.getDouble("mutationProbability")
    val evolutionDir = root.getString("evolutionDir")

    // neuron defaults
    val neuronRoot = root.getConfig("neuronDefaults")
    val slope = neuronRoot.getDouble("defaultSlope")
    val threshold = neuronRoot.getDouble("defaultThreshold")
    val weight = SynapseTrait(neuronRoot.getString("defaultWeight"))
    val hushValue = HushValue(neuronRoot.getInt("defaultHushValue"))
    val forgetting = ForgetTrait(neuronRoot.getString("defaultForgetting"))
    val tickTime = neuronRoot.getLong("defaultTickTime")

    val neuronDefaults = NeuronDefaults(slope, threshold, weight, hushValue, forgetting, tickTime)

    // synapse genome defaults
    val synapseDefaultsRoot = root.getConfig("synapseGenomeDefaults")
    val weightRange = synapseDefaultsRoot.getDouble("weightRange.from") <=> synapseDefaultsRoot.getDouble("weightRange.to")
    val hushProbability = Probability(synapseDefaultsRoot.getDouble("hushProbability"))
    val fullWeightProbability = Probability(synapseDefaultsRoot.getDouble("fullWeightProbability"))

    val synapseGenomeDefaults = SynapseGenomeDefaults(weightRange, hushProbability, fullWeightProbability)

    // neuron genome defaults - ranges

    val neuronGenomeRoot = root.getConfig("neuronGenomeDefaults")

    val thresholdRange = neuronGenomeRoot.getDouble("thresholdRange.from") <=> neuronGenomeRoot.getDouble("thresholdRange.to")
    val slopeRange = neuronGenomeRoot.getDouble("slopeRange.from") <=> neuronGenomeRoot.getDouble("slopeRange.to")
    val hushRange = neuronGenomeRoot.getInt("hushRange.from") to neuronGenomeRoot.getInt("hushRange.to")
    val forgettingRange = neuronGenomeRoot.getDouble("forgettingRange.from") <=> neuronGenomeRoot.getDouble("forgettingRange.to")
    val tickTimeMultiplierRange = neuronGenomeRoot.getDouble("tickTimeMultiplierRange.from") <=> neuronGenomeRoot.getDouble("tickTimeMultiplierRange.to")

    // neuron genome defaults - probabilities
    val dontForgetProbability = Probability(neuronGenomeRoot.getDouble("dontForgetProbability"))
    val forgetAllProbability = Probability(neuronGenomeRoot.getDouble("forgetAllProbability"))
    val thresholdProbability = Probability(neuronGenomeRoot.getDouble("thresholdProbability"))
    val slopeProbability = Probability(neuronGenomeRoot.getDouble("slopeProbability"))
    val forgettingProbability = Probability(neuronGenomeRoot.getDouble("forgettingProbability"))
    val hushValueProbability = Probability(neuronGenomeRoot.getDouble("hushValueProbability"))
    val tickTimeMultiplierProbability = Probability(neuronGenomeRoot.getDouble("tickTimeMultiplierProbability"))

    // neuron genome defaults - probabilities of mutating a synapse
    val synapseChangeProbability = Probability(neuronGenomeRoot.getDouble("synapseChangeProbability"))
    val addSynapseProbability = Probability(neuronGenomeRoot.getDouble("addSynapseProbability"))
    val deleteSynapseProbability = Probability(neuronGenomeRoot.getDouble("deleteSynapseProbability"))

    val neuronGenomeDefaults = NeuronGenomeDefaults(
      thresholdRange, slopeRange, hushRange, forgettingRange, tickTimeMultiplierRange,
      dontForgetProbability, forgetAllProbability, thresholdProbability,
      slopeProbability, forgettingProbability, hushValueProbability, tickTimeMultiplierProbability,
      synapseChangeProbability, addSynapseProbability, deleteSynapseProbability
    )

    // net genome defaults
    val netGenomeRoot = root.getConfig("netGenomeDefaults")

    val addNeuronProbability = Probability(netGenomeRoot.getDouble("addNeuronProbability"))
    val deleteNeuronProbability = Probability(netGenomeRoot.getDouble("deleteNeuronProbability"))
    val mutateNeuronProbability = Probability(netGenomeRoot.getDouble("mutateNeuronProbability"))
    val inputTickMultiplierProbability = Probability(netGenomeRoot.getDouble("inputTickMultiplierProbability"))
    val inputTickMultiplierRange = netGenomeRoot.getDouble("inputTickMultiplierRange.from") <=> netGenomeRoot.getDouble("inputTickMultiplierRange.to")

    val neuronsRange = netGenomeRoot.getInt("neuronsRange.from") to netGenomeRoot.getInt("neuronsRange.to")
    val synapsesDensity = netGenomeRoot.getDouble("synapsesDensity")

    val netGenomeDefaults = NetGenomeDefaults(
      addNeuronProbability, deleteNeuronProbability, mutateNeuronProbability, inputTickMultiplierProbability,
      inputTickMultiplierRange, neuronsRange, synapsesDensity
    )

    set(Context(awaitTimeout, initialMutationsNumber, genomePollSize, exercisesSetDir, mutationProbability, evolutionDir,
                neuronDefaults, synapseGenomeDefaults, neuronGenomeDefaults, netGenomeDefaults))
  }

  def fromJson(jsonStr: String) = read[Context](jsonStr)
  def withJson(jsonStr: String) = set(fromJson(jsonStr))
}