package anna

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.util.Timeout
import anna.data._
import anna.utils.Utils.formats
import com.typesafe.config.ConfigFactory
import org.json4s.native.Serialization.{read, writePretty}

import anna.logger.LOG._

import scala.concurrent.duration._

case class NeuronDefaults(
  threshold: Double,
  weight: SynapseTrait,
  hushValue: HushValue,
  tickTime: Long
){
  def toJson = writePretty(this)
}

case class Context(
  awaitTimeout: Long,
  neuronDefaults: NeuronDefaults
){
  def timeout = Timeout(FiniteDuration.apply(awaitTimeout, TimeUnit.SECONDS))
  def threshold = neuronDefaults.threshold
  def weight = neuronDefaults.weight
  def hushValue = neuronDefaults.hushValue
  def tickTime = neuronDefaults.tickTime

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

  def withThreshold(threshold: Double) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(threshold = threshold)))
  def withWeight(weight: SynapseTrait) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(weight = weight)))
  def withHushValue(hushValue: HushValue) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(hushValue = hushValue)))
  def withTickTime(tickTime: Long) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(tickTime = tickTime)))

  val _awaittimeout = "awaitTimeout"
  val _neurondefaults = "neuronDefaults"
  val _defaultthreshold = "defaultThreshold"
  val _defaultweight = "defaultWeight"
  val _defaulthushvalue = "defaultHushValue"
  val _defaultticktime = "defaultTickTime"

  private def init(): Unit ={
    val config = ConfigFactory.load()
    val root = config.getConfig("context")

    val awaitTimeout = root.getInt(_awaittimeout)

    // neuron defaults
    val neuronRoot = root.getConfig(_neurondefaults)
    val threshold = neuronRoot.getDouble(_defaultthreshold)
    val weight = SynapseTrait(neuronRoot.getString(_defaultweight))
    val hushValue = HushValue(neuronRoot.getInt(_defaulthushvalue))
    val tickTime = neuronRoot.getLong(_defaultticktime)

    val neuronDefaults = NeuronDefaults(threshold, weight, hushValue, tickTime)

    set(Context(awaitTimeout, neuronDefaults))
  }

  def fromJson(jsonStr: String) = read[Context](jsonStr)
  def withJson(jsonStr: String) = set(fromJson(jsonStr))

  def set(name: String, n: Int):Unit = name match {
    case `_defaulthushvalue` => withHushValue(HushValue(n))
  }
  
  def set(name: String, d: Double):Unit = name match {
    case `_defaultthreshold` => withThreshold(d)
    case `_defaultweight` => withWeight(SynapseWeight(d))
  }

  def set(map: Map[String,Any]):Unit = map.foreach(tuple =>
    if(tuple._2.isInstanceOf[Double]) set(tuple._1, tuple._2.asInstanceOf[Double])
    else if(tuple._2.isInstanceOf[Int]) set(tuple._1, tuple._2.asInstanceOf[Int])
    else exception(this,s"Unsuppored type of ${tuple._1}: ${tuple._2.getClass}")
  )
}