package anna

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.util.Timeout
import anna.data._
import anna.epengine.Probability
import anna.utils.DoubleRange
import com.typesafe.config.ConfigFactory
import anna.utils.DoubleRange._

import scala.concurrent.duration._

case class Context(
  timeout: Timeout,
  system: ActorSystem,

  slope: Double,
  threshold: Double,
  weight: SynapseWeight,
  hushValue: HushValue,
  forgetting: ForgetTrait,
  tickTime: Long,

  weightRange: DoubleRange,
  hushProbability: Probability,
  fullWeightProbability: Probability,

  thresholdRange: DoubleRange,
  slopeRange: DoubleRange,
  hushRange: Range,
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
)

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

  def withWeightRange(weightRange: DoubleRange) = set(apply().copy(weightRange = weightRange))
  def withHushProbability(hushProbability: Probability) = set(apply().copy(hushProbability = hushProbability))
  def withFullWeightProbability(fullWeightProbability: Probability) = set(apply().copy(fullWeightProbability = fullWeightProbability))

  def withThresholdRange(thresholdRange: DoubleRange) = set(apply().copy(thresholdRange = thresholdRange))
  def withSlopeRange(slopeRange: DoubleRange) = set(apply().copy(slopeRange = slopeRange))
  def withHushRange(hushRange: Range) = set(apply().copy(hushRange = hushRange))
  def withForgettingRange(forgettingRange: DoubleRange) = set(apply().copy(forgettingRange = forgettingRange))
  def withTickTimeMultiplierRange(tickTimeMultiplierRange: DoubleRange) = set(apply().copy(tickTimeMultiplierRange = tickTimeMultiplierRange))

  def withDontForgetProbability(dontForgetProbability: Probability) = set(apply().copy(dontForgetProbability = dontForgetProbability))
  def withForgetAllProbability(forgetAllProbability: Probability) = set(apply().copy(forgetAllProbability = forgetAllProbability))
  def withThresholdProbability(thresholdProbability: Probability) = set(apply().copy(thresholdProbability = thresholdProbability))
  def withSlopeProbability(slopeProbability: Probability) = set(apply().copy(slopeProbability = slopeProbability))
  def withForgettingProbability(forgettingProbability: Probability) = set(apply().copy(forgettingProbability = forgettingProbability))
  def withHushValueProbability(hushValueProbability: Probability) = set(apply().copy(hushValueProbability = hushValueProbability))
  def withTickTimeMultiplierProbability(tickTimeMultiplierProbability: Probability) = set(apply().copy(tickTimeMultiplierProbability = tickTimeMultiplierProbability))

  def withSynapseChangeProbability(synapseChangeProbability: Probability) = set(apply().copy(synapseChangeProbability = synapseChangeProbability))
  def withAddSynapseProbability(addSynapseProbability: Probability) = set(apply().copy(addSynapseProbability = addSynapseProbability))
  def withDeleteSynapseProbability(deleteSynapseProbability: Probability) = set(apply().copy(deleteSynapseProbability = deleteSynapseProbability))

  private def init(): Unit ={
    val config = ConfigFactory.load()
    val root = config.getConfig("context")

    implicit val timeout = Timeout(FiniteDuration.apply(root.getInt("awaitTimeout"), TimeUnit.SECONDS))

    lazy val system = ActorSystem("system")

    // neuron defaults
    val slope = root.getDouble("defaultSlope")
    val threshold = root.getDouble("defaultThreshold")
    val weight = SynapseWeight(root.getDouble("defaultWeight"))
    val hushValue = HushValue(root.getInt("defaultHushValue"))
    val forgetting = root.getString("defaultForgetting") match {
      case "DontForget" => DontForget
      case "ForgetAll" => ForgetAll
      case str => ForgetValue(str.toDouble)
    }
    val tickTime = root.getLong("defaultTickTime")

    // synapse genome defaults
    val weightRange = root.getDouble("weightRange.from") <=> root.getDouble("weightRange.to")
    val hushProbability = Probability(root.getDouble("hushProbability"))
    val fullWeightProbability = Probability(root.getDouble("fullWeightProbability"))

    // neuron genome defaults - ranges
    val thresholdRange = root.getDouble("thresholdRange.from") <=> root.getDouble("thresholdRange.to")
    val slopeRange = root.getDouble("slopeRange.from") <=> root.getDouble("slopeRange.to")
    val hushRange = root.getInt("hushRange.from") to root.getInt("hushRange.to")
    val forgettingRange = root.getDouble("forgettingRange.from") <=> root.getDouble("forgettingRange.to")
    val tickTimeMultiplierRange = root.getDouble("tickTimeMultiplierRange.from") <=> root.getDouble("tickTimeMultiplierRange.to")

    // neuron genome defaults - probabilities
    val forgetAllProbability = Probability(root.getDouble("forgetAllProbability"))
    val dontForgetProbability = Probability(root.getDouble("dontForgetProbability"))
    val thresholdProbability = Probability(root.getDouble("thresholdProbability"))
    val slopeProbability = Probability(root.getDouble("slopeProbability"))
    val forgettingProbability = Probability(root.getDouble("forgettingProbability"))
    val hushValueProbability = Probability(root.getDouble("hushValueProbability"))
    val tickTimeMultiplierProbability = Probability(root.getDouble("tickTimeMultiplierProbability"))

    // neuron genome defaults - probabilities of mutating a synapse
    val synapseChangeProbability = Probability(root.getDouble("synapseChangeProbability"))
    val addSynapseProbability = Probability(root.getDouble("addSynapseProbability"))
    val deleteSynapseProbability = Probability(root.getDouble("deleteSynapseProbability"))

    set(Context(timeout, system, slope, threshold, weight, hushValue, forgetting, tickTime,
                weightRange, hushProbability, fullWeightProbability,
                thresholdRange, slopeRange, hushRange, forgettingRange, tickTimeMultiplierRange,
                dontForgetProbability, forgetAllProbability, thresholdProbability, slopeProbability,
                forgettingProbability, hushValueProbability, tickTimeMultiplierProbability,
                synapseChangeProbability, addSynapseProbability, deleteSynapseProbability
    ))
  }

}