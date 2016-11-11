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

case class NeuronDefaults(threshold: Double, weight: SynapseTrait, silenceIterations: Int, iterationTime: Long){
  def toJson = writePretty(this)
}

case class Context(awaitTimeout: Long, maxRunIterations: Long, neuronDefaults: NeuronDefaults){
  def timeout = Timeout(FiniteDuration.apply(awaitTimeout, TimeUnit.SECONDS))
  def threshold = neuronDefaults.threshold
  def weight = neuronDefaults.weight
  def silenceIterations = neuronDefaults.silenceIterations
  def iterationTime = neuronDefaults.iterationTime

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

  def withAwaitTimeout(timeout: Long) =
    set(apply().copy(awaitTimeout = timeout))
  def withMaxRunIterations(iterations: Long) =
    set(apply().copy(maxRunIterations = iterations))
  def withThreshold(threshold: Double) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(threshold = threshold)))
  def withWeight(weight: SynapseTrait) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(weight = weight)))
  def withSilenceIterations(silenceIterations: Int) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(silenceIterations = silenceIterations)))
  def withIterationTime(iterationTime: Long) =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(iterationTime = iterationTime)))

  val _awaittimeout = "awaitTimeout"
  val _maxruniterations = "maxRunIterations"
  val _neurondefaults = "neuronDefaults"
  val _defaultthreshold = "defaultThreshold"
  val _defaultweight = "defaultWeight"
  val _defaultsilenceiterations = "defaultSilenceIterations"
  val _defaultiterationtime = "defaultIterationTime"

  private def init(): Unit ={
    val config = ConfigFactory.load()
    val root = config.getConfig("context")

    val awaitTimeout = root.getInt(_awaittimeout)
    val maxRunIterations = root.getInt(_maxruniterations)

    // neuron defaults
    val neuronRoot = root.getConfig(_neurondefaults)
    val threshold = neuronRoot.getDouble(_defaultthreshold)
    val weight = SynapseTrait(neuronRoot.getString(_defaultweight))
    val silenceIterations = neuronRoot.getInt(_defaultsilenceiterations)
    val iterationTime = neuronRoot.getLong(_defaultiterationtime)

    val neuronDefaults = NeuronDefaults(threshold, weight, silenceIterations, iterationTime)

    set(Context(awaitTimeout, maxRunIterations, neuronDefaults))
  }

  def fromJson(jsonStr: String) = read[Context](jsonStr)
  def withJson(jsonStr: String) = set(fromJson(jsonStr))

  def set(name: String, n: Int):Unit = name match {
    case `_awaittimeout` => withAwaitTimeout(n)
    case `_maxruniterations` => withMaxRunIterations(n)
    case `_defaultsilenceiterations` => withSilenceIterations(n)
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