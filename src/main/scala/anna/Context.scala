package anna

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.util.Timeout
import anna.data._
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._

object Context {
  private val config = ConfigFactory.load()
  private val root = config.getConfig("context")

  implicit val timeout = Timeout(FiniteDuration.apply(root.getInt("awaitTimeout"), TimeUnit.SECONDS))

  lazy val system = ActorSystem("system")
  
  // neuron
  var slope = root.getDouble("defaultSlope")
  var threshold = root.getDouble("defaultThreshold")
  var weight = SynapseWeight(root.getDouble("defaultWeight"))
  var hushValue = HushValue(root.getInt("defaultHushValue"))
  var forgetting = root.getString("defaultForgetting") match {
    case "DontForget" => DontForget
    case "ForgetAll" => ForgetAll
    case str => ForgetValue(str.toDouble)
  }
  var tickTime = root.getLong("defaultTickTime")


}