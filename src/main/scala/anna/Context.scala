package anna

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.util.Timeout
import anna.data._
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._

case class Context(
  timeout: Timeout,
  system: ActorSystem,
  slope: Double,
  threshold: Double,
  weight: SynapseWeight,
  hushValue: HushValue,
  forgetting: ForgetTrait,
  tickTime: Long
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

    instance = Some(Context(timeout, system, slope, threshold, weight, hushValue, forgetting, tickTime))
  }

}