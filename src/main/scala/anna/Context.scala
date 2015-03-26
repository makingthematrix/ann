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
  fullWeightProbability: Probability
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

  def withWeightRange(weightRange: DoubleRange) = {
    val t = apply()
    set(Context(t.timeout, t.system, t.slope, t.threshold, t.weight, t.hushValue, t.forgetting, t.tickTime,
                weightRange, t.hushProbability, t.fullWeightProbability))
  }

  def withHushProbability(hushProbability: Probability) = {
    val t = apply()
    set(Context(t.timeout, t.system, t.slope, t.threshold, t.weight, t.hushValue, t.forgetting, t.tickTime,
                t.weightRange, hushProbability, t.fullWeightProbability))
  }

  def withFullWeightProbability(fullWeightProbability: Probability) = {
    val t = apply()
    set(Context(t.timeout, t.system, t.slope, t.threshold, t.weight, t.hushValue, t.forgetting, t.tickTime,
                t.weightRange, t.hushProbability, fullWeightProbability))
  }

  private def init(): Unit ={
    val config = ConfigFactory.load()
    val root = config.getConfig("context")

    implicit val timeout = Timeout(FiniteDuration.apply(root.getInt("awaitTimeout"), TimeUnit.SECONDS))

    lazy val system = ActorSystem("system")

    // neuron
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

    val weightRange = root.getDouble("weightRange.from") <=> root.getDouble("weightRange.to")
    val hushProbability = Probability(root.getDouble("hushProbability"))
    val fullWeightProbability = Probability(root.getDouble("fullWeightProbability"))

    set(Context(timeout, system, slope, threshold, weight, hushValue, forgetting, tickTime,
                weightRange, hushProbability, fullWeightProbability))
  }

}