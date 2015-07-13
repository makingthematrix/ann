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
  tickTime: Long,
  activationFunctionName: String
){
  def toJson = writePretty(this)
}

case class SynapseGenomeDefaults(
  weightRange: DoubleRange,
  hushProbability: Probability,
  fullWeightProbability: Probability,
  invertSynapseProbability: Probability
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
  invertNeuronProbability: Probability,

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

case class EngineDefaults(
  initialMutationsNumber: Int,
  genomePollSize: Int,
  exercisesSetDir: String,
  mutationProbability: Probability,
  evolutionDir: String,
  crossCoefficient: Probability // this is not a probability, but the rules are the same
){
  def toJson = writePretty(this)
}

case class DotLineExercisesDefaults(
  oneSignalGivesDotImportance: Double,
  twoSignalsGiveLineImportance: Double,
  oneSignalWithNoiseGivesDotImportance: Double,
  twoSignalsWithNoiseGiveLineImportance: Double,
  oneVariedSignalGivesDotImportance: Double,
  twoVariedSignalsGiveLineImportance: Double,
  oneVariedSignalWithNoiseGivesDotImportance: Double,
  twoVariedSignalsWithNoiseGiveLineImportance: Double
){
  def toJson = writePretty(this)
}

case class Context(
  awaitTimeout: Long,

  engineDefaults: EngineDefaults,
  neuronDefaults: NeuronDefaults,
  synapseGenomeDefaults: SynapseGenomeDefaults,
  neuronGenomeDefaults: NeuronGenomeDefaults,
  netGenomeDefaults: NetGenomeDefaults,
  dotLineExercisesDefaults: DotLineExercisesDefaults
){
  def timeout = Timeout(FiniteDuration.apply(awaitTimeout, TimeUnit.SECONDS))

  def system:ActorSystem = {
    implicit lazy val t = timeout
    lazy val s = ActorSystem("system")
    val p = () => s
    p()
  }

  def toJson = writePretty(this)

  def initialMutationsNumber = engineDefaults.initialMutationsNumber
  def genomePollSize = engineDefaults.genomePollSize
  def exercisesSetDir = engineDefaults.exercisesSetDir
  def mutationProbability = engineDefaults.mutationProbability
  def evolutionDir = engineDefaults.evolutionDir
  def crossCoefficient = engineDefaults.crossCoefficient

  def slope = neuronDefaults.slope
  def threshold = neuronDefaults.threshold
  def weight = neuronDefaults.weight
  def hushValue = neuronDefaults.hushValue
  def forgetting = neuronDefaults.forgetting
  def tickTime = neuronDefaults.tickTime
  def activationFunctionName = neuronDefaults.activationFunctionName

  def weightRange = synapseGenomeDefaults.weightRange
  def hushProbability = synapseGenomeDefaults.hushProbability
  def fullWeightProbability = synapseGenomeDefaults.fullWeightProbability
  def invertSynapseProbability = synapseGenomeDefaults.invertSynapseProbability

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
  def invertNeuronProbability = neuronGenomeDefaults.invertNeuronProbability


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

  def oneSignalGivesDotImportance = dotLineExercisesDefaults.oneSignalGivesDotImportance
  def twoSignalsGiveLineImportance = dotLineExercisesDefaults.twoSignalsGiveLineImportance
  def oneSignalWithNoiseGivesDotImportance = dotLineExercisesDefaults.oneSignalWithNoiseGivesDotImportance
  def twoSignalsWithNoiseGiveLineImportance = dotLineExercisesDefaults.twoSignalsWithNoiseGiveLineImportance
  def oneVariedSignalGivesDotImportance = dotLineExercisesDefaults.oneVariedSignalGivesDotImportance
  def twoVariedSignalsGiveLineImportance = dotLineExercisesDefaults.twoVariedSignalsGiveLineImportance
  def oneVariedSignalWithNoiseGivesDotImportance = dotLineExercisesDefaults.oneVariedSignalWithNoiseGivesDotImportance
  def twoVariedSignalsWithNoiseGiveLineImportance = dotLineExercisesDefaults.twoVariedSignalsWithNoiseGiveLineImportance
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

  def withInitialMutationsNumber(initialMutationsNumber: Int) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(initialMutationsNumber = initialMutationsNumber)))
  def withGenomePollSize(genomePollSize: Int) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(genomePollSize = genomePollSize)))
  def withExercisesSetDir(exercisesSetDir: String) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(exercisesSetDir = exercisesSetDir)))
  def withMutationProbability(mutationProbability: Double) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(mutationProbability = mutationProbability)))
  def withEvolutionDir(evolutionDir: String) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(evolutionDir = evolutionDir)))
  def withCrossCoefficient(crossCoefficient: Double) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(crossCoefficient = crossCoefficient)))

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
  def withActivationFunctionName(activationFunctionName: String) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(activationFunctionName = activationFunctionName)))

  def withOneSignalGivesDotImportance(oneSignalGivesDotImportance: Double) =
    set(apply().copy(dotLineExercisesDefaults = that.dotLineExercisesDefaults.copy(oneSignalGivesDotImportance = oneSignalGivesDotImportance)))
  def withTwoSignalsGiveLineImportance(twoSignalsGiveLineImportance: Double) =
    set(apply().copy(dotLineExercisesDefaults = that.dotLineExercisesDefaults.copy(twoSignalsGiveLineImportance = twoSignalsGiveLineImportance)))
  def withOneSignalWithNoiseGivesDotImportance(oneSignalWithNoiseGivesDotImportance: Double) =
    set(apply().copy(dotLineExercisesDefaults = that.dotLineExercisesDefaults.copy(oneSignalWithNoiseGivesDotImportance = oneSignalWithNoiseGivesDotImportance)))
  def withTwoSignalsWithNoiseGiveLineImportance(twoSignalsWithNoiseGiveLineImportance: Double) =
    set(apply().copy(dotLineExercisesDefaults = that.dotLineExercisesDefaults.copy(twoSignalsWithNoiseGiveLineImportance = twoSignalsWithNoiseGiveLineImportance)))
  def withOneVariedSignalGivesDotImportance(oneVariedSignalGivesDotImportance: Double) =
    set(apply().copy(dotLineExercisesDefaults = that.dotLineExercisesDefaults.copy(oneVariedSignalGivesDotImportance = oneVariedSignalGivesDotImportance)))
  def withTwoVariedSignalsGiveLineImportance(twoVariedSignalsGiveLineImportance: Double) =
    set(apply().copy(dotLineExercisesDefaults = that.dotLineExercisesDefaults.copy(twoVariedSignalsGiveLineImportance = twoVariedSignalsGiveLineImportance)))
  def withOneVariedSignalWithNoiseGivesDotImportance(oneVariedSignalWithNoiseGivesDotImportance: Double) =
    set(apply().copy(dotLineExercisesDefaults = that.dotLineExercisesDefaults.copy(oneVariedSignalWithNoiseGivesDotImportance = oneVariedSignalWithNoiseGivesDotImportance)))
  def withTwoVariedSignalsWithNoiseGiveLineImportance(twoVariedSignalsWithNoiseGiveLineImportance: Double) =
    set(apply().copy(dotLineExercisesDefaults = that.dotLineExercisesDefaults.copy(twoVariedSignalsWithNoiseGiveLineImportance = twoVariedSignalsWithNoiseGiveLineImportance)))

  private def init(): Unit ={
    val config = ConfigFactory.load()
    val root = config.getConfig("context")

    val awaitTimeout = root.getInt("awaitTimeout")

    val engineRoot = root.getConfig("engineDefaults")
    val initialMutationsNumber = engineRoot.getInt("initialMutationsNumber")
    val genomePollSize = engineRoot.getInt("genomePollSize")
    val exercisesSetDir = engineRoot.getString("exercisesSetDir")
    val mutationProbability = engineRoot.getDouble("mutationProbability")
    val evolutionDir = engineRoot.getString("evolutionDir")
    val crossCoefficient = engineRoot.getDouble("crossCoefficient")

    val engineDefaults = EngineDefaults(
      initialMutationsNumber, genomePollSize, exercisesSetDir, mutationProbability, evolutionDir, crossCoefficient
    )

    // neuron defaults
    val neuronRoot = root.getConfig("neuronDefaults")
    val slope = neuronRoot.getDouble("defaultSlope")
    val threshold = neuronRoot.getDouble("defaultThreshold")
    val weight = SynapseTrait(neuronRoot.getString("defaultWeight"))
    val hushValue = HushValue(neuronRoot.getInt("defaultHushValue"))
    val forgetting = ForgetTrait(neuronRoot.getString("defaultForgetting"))
    val tickTime = neuronRoot.getLong("defaultTickTime")
    val activationFunctionName = neuronRoot.getString("activationFunctionName")

    val neuronDefaults = NeuronDefaults(slope, threshold, weight, hushValue, forgetting, tickTime, activationFunctionName)

    // synapse genome defaults
    val synapseDefaultsRoot = root.getConfig("synapseGenomeDefaults")
    val weightRange = synapseDefaultsRoot.getDouble("weightRange.from") <=> synapseDefaultsRoot.getDouble("weightRange.to")
    val hushProbability = Probability(synapseDefaultsRoot.getDouble("hushProbability"))
    val fullWeightProbability = Probability(synapseDefaultsRoot.getDouble("fullWeightProbability"))
    val invertSynapseProbability = Probability(synapseDefaultsRoot.getDouble("invertSynapseProbability"))

    val synapseGenomeDefaults = SynapseGenomeDefaults(weightRange, hushProbability, fullWeightProbability, invertSynapseProbability)

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
    val invertNeuronProbability = Probability(neuronGenomeRoot.getDouble("invertNeuronProbability"))

    // neuron genome defaults - probabilities of mutating a synapse
    val synapseChangeProbability = Probability(neuronGenomeRoot.getDouble("synapseChangeProbability"))
    val addSynapseProbability = Probability(neuronGenomeRoot.getDouble("addSynapseProbability"))
    val deleteSynapseProbability = Probability(neuronGenomeRoot.getDouble("deleteSynapseProbability"))

    val neuronGenomeDefaults = NeuronGenomeDefaults(
      thresholdRange, slopeRange, hushRange, forgettingRange, tickTimeMultiplierRange,
      dontForgetProbability, forgetAllProbability, thresholdProbability,
      slopeProbability, forgettingProbability, hushValueProbability, tickTimeMultiplierProbability,
      invertNeuronProbability,
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

    // dot-line exercises defaults
    val dotLineExercisesRoot = root.getConfig("dotLineExercisesDefaults")

    val oneSignalGivesDotImportance = dotLineExercisesRoot.getDouble("oneSignalGivesDotImportance")
    val twoSignalsGiveLineImportance = dotLineExercisesRoot.getDouble("twoSignalsGiveLineImportance")
    val oneSignalWithNoiseGivesDotImportance = dotLineExercisesRoot.getDouble("oneSignalWithNoiseGivesDotImportance")
    val twoSignalsWithNoiseGiveLineImportance = dotLineExercisesRoot.getDouble("twoSignalsWithNoiseGiveLineImportance")
    val oneVariedSignalGivesDotImportance = dotLineExercisesRoot.getDouble("oneVariedSignalGivesDotImportance")
    val twoVariedSignalsGiveLineImportance = dotLineExercisesRoot.getDouble("twoVariedSignalsGiveLineImportance")
    val oneVariedSignalWithNoiseGivesDotImportance = dotLineExercisesRoot.getDouble("oneVariedSignalWithNoiseGivesDotImportance")
    val twoVariedSignalsWithNoiseGiveLineImportance = dotLineExercisesRoot.getDouble("twoVariedSignalsWithNoiseGiveLineImportance")

    val dotLineExercisesDefaults = DotLineExercisesDefaults(
      oneSignalGivesDotImportance,
      twoSignalsGiveLineImportance,
      oneSignalWithNoiseGivesDotImportance,
      twoSignalsWithNoiseGiveLineImportance,
      oneVariedSignalGivesDotImportance,
      twoVariedSignalsGiveLineImportance,
      oneVariedSignalWithNoiseGivesDotImportance,
      twoVariedSignalsWithNoiseGiveLineImportance
    )

    set(Context(awaitTimeout, engineDefaults, neuronDefaults, synapseGenomeDefaults,
                neuronGenomeDefaults, netGenomeDefaults, dotLineExercisesDefaults
    ))
  }

  def fromJson(jsonStr: String) = read[Context](jsonStr)
  def withJson(jsonStr: String) = set(fromJson(jsonStr))
}