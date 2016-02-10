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

import anna.logger.LOG._

import scala.concurrent.duration._

case class FireWithDelayDefaults(
  addProbability: Probability,
  deleteProbability: Probability,
  modifyProbability: Probability,
  delayRange: IntRange
){
  def toJson = writePretty(this)
}

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
  mutationsPerGenome: IntRange,
  evolutionDir: String,
  crossCoefficient: Probability, // this is not a probability, but the rules are the same
  shufflingCoefficient: Probability // this too
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
  dotLineExercisesDefaults: DotLineExercisesDefaults,
  fireWithDelayDefaults: FireWithDelayDefaults
){
  def timeout = Timeout(FiniteDuration.apply(awaitTimeout, TimeUnit.SECONDS))

  private var systemOpt: Option[ActorSystem] = None

  def system:ActorSystem = systemOpt match {
    case Some(actorSystem) => actorSystem
    case None =>
      implicit val t = timeout
      val actorSystem = ActorSystem("system")
      systemOpt = Some(actorSystem)
      actorSystem
  }

  def shutdownSystem() = if(systemOpt != None) {
    systemOpt.get.shutdown()
    systemOpt = None
  }

  def toJson = writePretty(this)

  def initialMutationsNumber = engineDefaults.initialMutationsNumber
  def genomePollSize = engineDefaults.genomePollSize
  def exercisesSetDir = engineDefaults.exercisesSetDir
  def mutationProbability = engineDefaults.mutationProbability
  def mutationsPerGenome = engineDefaults.mutationsPerGenome
  def evolutionDir = engineDefaults.evolutionDir
  def crossCoefficient = engineDefaults.crossCoefficient
  def shufflingCoefficient = engineDefaults.shufflingCoefficient

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

  def fwdAddProbability = fireWithDelayDefaults.addProbability
  def fwdDeleteProbability = fireWithDelayDefaults.deleteProbability
  def fwdModifyProbability = fireWithDelayDefaults.modifyProbability
  def fwdDelayRange = fireWithDelayDefaults.delayRange
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

  private final def that = instance.get

  def withInitialMutationsNumber(initialMutationsNumber: Int) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(initialMutationsNumber = initialMutationsNumber)))
  def withGenomePollSize(genomePollSize: Int) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(genomePollSize = genomePollSize)))
  def withExercisesSetDir(exercisesSetDir: String) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(exercisesSetDir = exercisesSetDir)))
  def withMutationProbability(mutationProbability: Double) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(mutationProbability = mutationProbability)))
  def withMutationsPerGenome(mutationsPerGenome: Range) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(mutationsPerGenome = mutationsPerGenome)))
  def withEvolutionDir(evolutionDir: String) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(evolutionDir = evolutionDir)))
  def withCrossCoefficient(crossCoefficient: Double) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(crossCoefficient = crossCoefficient)))
  def withShufflingCoefficient(shufflingCoefficient: Double) =
    set(apply().copy(engineDefaults = that.engineDefaults.copy(shufflingCoefficient = shufflingCoefficient)))

  def withWeightRange(weightRange: DoubleRange) =
    set(apply().copy(synapseGenomeDefaults = that.synapseGenomeDefaults.copy(weightRange = weightRange)))
  def withHushProbability(hushProbability: Probability) =
    set(apply().copy(synapseGenomeDefaults = that.synapseGenomeDefaults.copy(hushProbability = hushProbability)))
  def withFullWeightProbability(fullWeightProbability: Probability) =
    set(apply().copy(synapseGenomeDefaults = that.synapseGenomeDefaults.copy(fullWeightProbability = fullWeightProbability)))
  def withInvertSynapseProbability(invertSynapseProbability: Probability) =
    set(apply().copy(synapseGenomeDefaults = that.synapseGenomeDefaults.copy(invertSynapseProbability = invertSynapseProbability)))

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
  def withInvertNeuronProbability(invertNeuronProbability: Probability) =
    set(apply().copy(neuronGenomeDefaults = that.neuronGenomeDefaults.copy(invertNeuronProbability = invertNeuronProbability)))
  
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

  def withFwdAddProbability(addProbability: Probability) =
    set(apply().copy(fireWithDelayDefaults = that.fireWithDelayDefaults.copy(addProbability = addProbability)))
  def withFwdDeleteProbability(deleteProbability: Probability) =
    set(apply().copy(fireWithDelayDefaults = that.fireWithDelayDefaults.copy(deleteProbability = deleteProbability)))
  def withFwdModifyProbability(modifyProbability: Probability) =
    set(apply().copy(fireWithDelayDefaults = that.fireWithDelayDefaults.copy(modifyProbability = modifyProbability)))
  def withFwdDelayRange(delayRange: IntRange) =
    set(apply().copy(fireWithDelayDefaults = that.fireWithDelayDefaults.copy(delayRange = delayRange)))
  def withFwdDelay(delay: Int) =
    set(apply().copy(fireWithDelayDefaults = that.fireWithDelayDefaults.copy(delayRange = delay to delay)))

  val _awaittimeout = "awaitTimeout"
  val _enginedefaults = "engineDefaults"
  val _initialmutationsnumber = "initialMutationsNumber"
  val _genomepollsize = "genomePollSize"
  val _exercisessetdir = "exercisesSetDir"
  val _mutationprobability = "mutationProbability"
  val _mutationspergenome = "mutationsPerGenome"
  val _mutationspergenomefrom = "mutationsPerGenome.from"
  val _mutationspergenometo = "mutationsPerGenome.to"
  val _evolutiondir = "evolutionDir"
  val _crosscoefficient = "crossCoefficient"
  val _shufflingcoefficient = "shufflingCoefficient"
  val _neurondefaults = "neuronDefaults"
  val _defaultslope = "defaultSlope"
  val _defaultthreshold = "defaultThreshold"
  val _defaultweight = "defaultWeight"
  val _defaulthushvalue = "defaultHushValue"
  val _defaultforgetting = "defaultForgetting"
  val _defaultticktime = "defaultTickTime"
  val _activationfunctionname = "activationFunctionName"
  val _synapsegenomedefaults = "synapseGenomeDefaults"
  val _weightrange = "weightRange"
  val _weightrangefrom = "weightRange.from"
  val _weightrangeto = "weightRange.to"
  val _hushprobability = "hushProbability"
  val _fullweightprobability = "fullWeightProbability"
  val _invertsynapseprobability = "invertSynapseProbability"
  val _neurongenomedefaults = "neuronGenomeDefaults"
  val _thresholdrange = "thresholdRange"
  val _thresholdrangefrom = "thresholdRange.from"
  val _thresholdrangeto = "thresholdRange.to"
  val _sloperange = "slopeRange"
  val _sloperangefrom = "slopeRange.from"
  val _sloperangeto = "slopeRange.to"
  val _hushrange = "hushRange"
  val _hushrangefrom = "hushRange.from"
  val _hushrangeto = "hushRange.to"
  val _forgettingrange = "forgettingRange"
  val _forgettingrangefrom = "forgettingRange.from"
  val _forgettingrangeto = "forgettingRange.to"
  val _tickmultiplierrange = "tickTimeMultiplierRange"
  val _tickmultiplierrangefrom = "tickTimeMultiplierRange.from"
  val _ticktimemultiplierrangeto = "tickTimeMultiplierRange.to"
  val _dontforgetprobability = "dontForgetProbability"
  val _forgetallprobability = "forgetAllProbability"
  val _thresholdprobability = "thresholdProbability"
  val _slopeprobability = "slopeProbability"
  val _forgettingprobability = "forgettingProbability"
  val _hushvalueprobability = "hushValueProbability"
  val _ticktimemultiplierprobability = "tickTimeMultiplierProbability"
  val _invertneuronprobability = "invertNeuronProbability"
  val _synapsechangeprobability = "synapseChangeProbability"
  val _addsynapseprobability = "addSynapseProbability"
  val _deletesynapseprobability = "deleteSynapseProbability"
  val _netgenomedefaults = "netGenomeDefaults"
  val _addneuronprobability = "addNeuronProbability"
  val _deleteneuronprobability = "deleteNeuronProbability"
  val _mutateneuronprobability = "mutateNeuronProbability"
  val _inputtickmultiplierprobability = "inputTickMultiplierProbability"
  val _inputtickmultiplierrange = "inputTickMultiplierRange"
  val _inputtickmultiplierrangefrom = "inputTickMultiplierRange.from"
  val _inputtickmultiplierrangeto = "inputTickMultiplierRange.to"
  val _neuronsrange = "neuronsRange"
  val _neuronsrangefrom = "neuronsRange.from"
  val _neuronsrangeto = "neuronsRange.to"
  val _synapsesdensity = "synapsesDensity"
  val _dotlineexercisesdefaults = "dotLineExercisesDefaults"
  val _onesignalgivesdotimportance = "oneSignalGivesDotImportance"
  val _twosignalsgivelineimportance = "twoSignalsGiveLineImportance"
  val _onesignalwithnoisegivesdotimportance = "oneSignalWithNoiseGivesDotImportance"
  val _twosignalswithnoisegivelineimportance = "twoSignalsWithNoiseGiveLineImportance"
  val _onevariedsignalgivesdotimportance = "oneVariedSignalGivesDotImportance"
  val _twovariedsignalsgivelineimportance = "twoVariedSignalsGiveLineImportance"
  val _onevariedsignalwithnoisegivesdotimportance = "oneVariedSignalWithNoiseGivesDotImportance"
  val _twovariedsignalswithnoisegivelineimportance = "twoVariedSignalsWithNoiseGiveLineImportance"
  val _fwddefaults = "fireWithDelayDefaults"
  val _fwdaddprobability = "addProbability"
  val _fwddeleteprobability = "deleteProbability"
  val _fwdmodifyprobability = "modifyProbability"
  val _fwddelayrange = "delayRange"
  val _fwddelayrangefrom = "delayRange.from"
  val _fwddelayrangeto = "delayRange.to"

  private def init(): Unit ={
    val config = ConfigFactory.load()
    val root = config.getConfig("context")

    val awaitTimeout = root.getInt(_awaittimeout)

    val engineRoot = root.getConfig(_enginedefaults)
    val initialMutationsNumber = engineRoot.getInt(_initialmutationsnumber)
    val genomePollSize = engineRoot.getInt(_genomepollsize)
    val exercisesSetDir = engineRoot.getString(_exercisessetdir)
    val mutationProbability = engineRoot.getDouble(_mutationprobability)
    val mutationsPerGenome = engineRoot.getInt(_mutationspergenomefrom) to engineRoot.getInt(_mutationspergenometo)
    val evolutionDir = engineRoot.getString(_evolutiondir)
    val crossCoefficient = engineRoot.getDouble(_crosscoefficient)
    val shufflingCoefficient = engineRoot.getDouble(_shufflingcoefficient)

    val engineDefaults = EngineDefaults(
      initialMutationsNumber, genomePollSize, exercisesSetDir, mutationProbability,
      mutationsPerGenome, evolutionDir, crossCoefficient, shufflingCoefficient
    )

    // neuron defaults
    val neuronRoot = root.getConfig(_neurondefaults)
    val slope = neuronRoot.getDouble(_defaultslope)
    val threshold = neuronRoot.getDouble(_defaultthreshold)
    val weight = SynapseTrait(neuronRoot.getString(_defaultweight))
    val hushValue = HushValue(neuronRoot.getInt(_defaulthushvalue))
    val forgetting = ForgetTrait(neuronRoot.getString(_defaultforgetting))
    val tickTime = neuronRoot.getLong(_defaultticktime)
    val activationFunctionName = neuronRoot.getString(_activationfunctionname)

    val neuronDefaults = NeuronDefaults(slope, threshold, weight, hushValue, forgetting, tickTime, activationFunctionName)

    // synapse genome defaults
    val synapseDefaultsRoot = root.getConfig(_synapsegenomedefaults)
    val weightRange = synapseDefaultsRoot.getDouble(_weightrangefrom) <=> synapseDefaultsRoot.getDouble(_weightrangeto)
    val hushProbability = Probability(synapseDefaultsRoot.getDouble(_hushprobability))
    val fullWeightProbability = Probability(synapseDefaultsRoot.getDouble(_fullweightprobability))
    val invertSynapseProbability = Probability(synapseDefaultsRoot.getDouble(_invertsynapseprobability))

    val synapseGenomeDefaults = SynapseGenomeDefaults(weightRange, hushProbability, fullWeightProbability, invertSynapseProbability)

    // neuron genome defaults - ranges

    val neuronGenomeRoot = root.getConfig(_neurongenomedefaults)

    val thresholdRange = neuronGenomeRoot.getDouble(_thresholdrangefrom) <=> neuronGenomeRoot.getDouble(_thresholdrangeto)
    val slopeRange = neuronGenomeRoot.getDouble(_sloperangefrom) <=> neuronGenomeRoot.getDouble(_sloperangeto)
    val hushRange = neuronGenomeRoot.getInt(_hushrangefrom) to neuronGenomeRoot.getInt(_hushrangeto)
    val forgettingRange = neuronGenomeRoot.getDouble(_forgettingrangefrom) <=> neuronGenomeRoot.getDouble(_forgettingrangeto)
    val tickTimeMultiplierRange = neuronGenomeRoot.getDouble(_tickmultiplierrangefrom) <=> neuronGenomeRoot.getDouble(_ticktimemultiplierrangeto)

    // neuron genome defaults - probabilities
    val dontForgetProbability = Probability(neuronGenomeRoot.getDouble(_dontforgetprobability))
    val forgetAllProbability = Probability(neuronGenomeRoot.getDouble(_forgetallprobability))
    val thresholdProbability = Probability(neuronGenomeRoot.getDouble(_thresholdprobability))
    val slopeProbability = Probability(neuronGenomeRoot.getDouble(_slopeprobability))
    val forgettingProbability = Probability(neuronGenomeRoot.getDouble(_forgettingprobability))
    val hushValueProbability = Probability(neuronGenomeRoot.getDouble(_hushvalueprobability))
    val tickTimeMultiplierProbability = Probability(neuronGenomeRoot.getDouble(_ticktimemultiplierprobability))
    val invertNeuronProbability = Probability(neuronGenomeRoot.getDouble(_invertneuronprobability))

    // neuron genome defaults - probabilities of mutating a synapse
    val synapseChangeProbability = Probability(neuronGenomeRoot.getDouble(_synapsechangeprobability))
    val addSynapseProbability = Probability(neuronGenomeRoot.getDouble(_addsynapseprobability))
    val deleteSynapseProbability = Probability(neuronGenomeRoot.getDouble(_deletesynapseprobability))

    val neuronGenomeDefaults = NeuronGenomeDefaults(
      thresholdRange, slopeRange, hushRange, forgettingRange, tickTimeMultiplierRange,
      dontForgetProbability, forgetAllProbability, thresholdProbability,
      slopeProbability, forgettingProbability, hushValueProbability, tickTimeMultiplierProbability,
      invertNeuronProbability,
      synapseChangeProbability, addSynapseProbability, deleteSynapseProbability
    )

    // net genome defaults
    val netGenomeRoot = root.getConfig(_netgenomedefaults)

    val addNeuronProbability = Probability(netGenomeRoot.getDouble(_addneuronprobability))
    val deleteNeuronProbability = Probability(netGenomeRoot.getDouble(_deleteneuronprobability))
    val mutateNeuronProbability = Probability(netGenomeRoot.getDouble(_mutateneuronprobability))
    val inputTickMultiplierProbability = Probability(netGenomeRoot.getDouble(_inputtickmultiplierprobability))
    val inputTickMultiplierRange = netGenomeRoot.getDouble(_inputtickmultiplierrangefrom) <=> netGenomeRoot.getDouble(_inputtickmultiplierrangeto)

    val neuronsRange = netGenomeRoot.getInt(_neuronsrangefrom) to netGenomeRoot.getInt(_neuronsrangeto)
    val synapsesDensity = netGenomeRoot.getDouble(_synapsesdensity)

    val netGenomeDefaults = NetGenomeDefaults(
      addNeuronProbability, deleteNeuronProbability, mutateNeuronProbability, inputTickMultiplierProbability,
      inputTickMultiplierRange, neuronsRange, synapsesDensity
    )

    // dot-line exercises defaults
    val dotLineExercisesRoot = root.getConfig(_dotlineexercisesdefaults)

    val oneSignalGivesDotImportance = dotLineExercisesRoot.getDouble(_onesignalgivesdotimportance)
    val twoSignalsGiveLineImportance = dotLineExercisesRoot.getDouble(_twosignalsgivelineimportance)
    val oneSignalWithNoiseGivesDotImportance = dotLineExercisesRoot.getDouble(_onesignalwithnoisegivesdotimportance)
    val twoSignalsWithNoiseGiveLineImportance = dotLineExercisesRoot.getDouble(_twosignalswithnoisegivelineimportance)
    val oneVariedSignalGivesDotImportance = dotLineExercisesRoot.getDouble(_onevariedsignalgivesdotimportance)
    val twoVariedSignalsGiveLineImportance = dotLineExercisesRoot.getDouble(_twovariedsignalsgivelineimportance)
    val oneVariedSignalWithNoiseGivesDotImportance = dotLineExercisesRoot.getDouble(_onevariedsignalwithnoisegivesdotimportance)
    val twoVariedSignalsWithNoiseGiveLineImportance = dotLineExercisesRoot.getDouble(_twovariedsignalswithnoisegivelineimportance)

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

    // fire with delay block defaults
    val fwdRoot = root.getConfig(_fwddefaults)

    val fwdAddProbability = fwdRoot.getDouble(_fwdaddprobability)
    val fwdDeleteProbability = fwdRoot.getDouble(_fwddeleteprobability)
    val fwdModifyProbability = fwdRoot.getDouble(_fwdmodifyprobability)
    val fwdDelayRange = fwdRoot.getInt(_fwddelayrangefrom) to fwdRoot.getInt(_fwddelayrangeto)

    val fwdDefaults = FireWithDelayDefaults(fwdAddProbability, fwdDeleteProbability, fwdModifyProbability, fwdDelayRange)

    set(Context(awaitTimeout, engineDefaults, neuronDefaults, synapseGenomeDefaults,
                neuronGenomeDefaults, netGenomeDefaults, dotLineExercisesDefaults, fwdDefaults
    ))
  }

  def fromJson(jsonStr: String) = read[Context](jsonStr)
  def withJson(jsonStr: String) = set(fromJson(jsonStr))

  def set(name: String, r: Range):Unit = name match {
    case `_hushrange` => withHushRange(r)
    case `_neuronsrange` => withNeuronsRange(r)
    case `_mutationspergenome` => withMutationsPerGenome(r)
    case `_fwddelayrange` => withFwdDelayRange(r)
  }

  def set(name: String, r: DoubleRange):Unit = name match {
    case `_weightrange` => withWeightRange(r)
    case `_thresholdrange` => withThresholdRange(r)
    case `_sloperange` => withSlopeRange(r)
    case `_forgettingrange` => withForgettingRange(r)
  }

  def set(name: String, n: Int):Unit = name match {
    case `_genomepollsize` => withGenomePollSize(n)
    case `_defaulthushvalue` => withHushValue(HushValue(n))
  }
  
  def set(name: String, d: Double):Unit = name match {
    case `_mutationprobability` => withMutationProbability(d)
    case `_crosscoefficient` => withCrossCoefficient(d)
    case `_shufflingcoefficient` => withShufflingCoefficient(d)
    case `_defaultslope` => withSlope(d)
    case `_defaultthreshold` => withThreshold(d)
    case `_defaultweight` => withWeight(SynapseWeight(d))
    case `_defaultforgetting` => withForgetting(ForgetValue(d))
    case `_hushprobability` => withHushProbability(d)
    case `_fullweightprobability` => withFullWeightProbability(d)
    case `_invertsynapseprobability` => withInvertSynapseProbability(d)
    case `_dontforgetprobability` => withDontForgetProbability(d)
    case `_forgetallprobability` => withForgetAllProbability(d)
    case `_thresholdprobability` => withThresholdProbability(d)
    case `_slopeprobability` => withSlopeProbability(d)
    case `_forgettingprobability` => withForgettingProbability(d)
    case `_hushvalueprobability` => withHushValueProbability(d)
    case `_invertneuronprobability` => withInvertNeuronProbability(d)
    case `_synapsechangeprobability` => withSynapseChangeProbability(d)
    case `_addsynapseprobability` => withAddSynapseProbability(d)
    case `_deletesynapseprobability` => withDeleteSynapseProbability(d)
    case `_addneuronprobability` => withAddNeuronProbability(d)
    case `_deleteneuronprobability` => withDeleteNeuronProbability(d)
    case `_mutateneuronprobability` => withMutateNeuronProbability(d)
    case `_synapsesdensity` => withSynapsesDensity(d)
    case `_onesignalgivesdotimportance` => withOneSignalGivesDotImportance(d)
    case `_twosignalsgivelineimportance` => withTwoSignalsGiveLineImportance(d)
    case `_onesignalwithnoisegivesdotimportance` => withOneSignalWithNoiseGivesDotImportance(d)
    case `_twosignalswithnoisegivelineimportance` => withTwoSignalsWithNoiseGiveLineImportance(d)
    case `_onevariedsignalgivesdotimportance` => withOneVariedSignalGivesDotImportance(d)
    case `_twovariedsignalsgivelineimportance` => withTwoVariedSignalsGiveLineImportance(d)
    case `_onevariedsignalwithnoisegivesdotimportance` => withOneVariedSignalWithNoiseGivesDotImportance(d)
    case `_twovariedsignalswithnoisegivelineimportance` => withTwoVariedSignalsWithNoiseGiveLineImportance(d)
    case `_genomepollsize` => withGenomePollSize(d.toInt)
    case `_defaulthushvalue` => withHushValue(HushValue(d.toInt))
    case `_fwdaddprobability` => withFwdAddProbability(d)
    case `_fwddeleteprobability` => withFwdDeleteProbability(d)
    case `_fwdmodifyprobability` => withFwdModifyProbability(d)
  }

  def set(map: Map[String,Any]):Unit = map.foreach(tuple =>
    if(tuple._2.isInstanceOf[Double]) set(tuple._1, tuple._2.asInstanceOf[Double])
    else if(tuple._2.isInstanceOf[Int]) set(tuple._1, tuple._2.asInstanceOf[Int])
    else if(tuple._2.isInstanceOf[DoubleRange]) set(tuple._1, tuple._2.asInstanceOf[DoubleRange])
    else if(tuple._2.isInstanceOf[Range]) set(tuple._1, tuple._2.asInstanceOf[Range])
    else exception(this,s"Unsuppored type of ${tuple._1}: ${tuple._2.getClass}")
  )
}