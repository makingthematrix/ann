package anna

import java.util.concurrent.TimeUnit

import akka.actor.typed.{ActorSystem, SpawnProtocol}
import akka.util.Timeout
import anna.data._
import anna.utils.Utils.{formats, classTagToManifest}
import com.typesafe.config.ConfigFactory
import org.json4s.native.Serialization.{read, writePretty}
import scala.reflect.ClassTag

import scala.concurrent.duration._

final case class NeuronDefaults(threshold: Double, weight: SynapseTrait, silenceIterations: Int, iterationTime: Long){
  def toJson: String = writePretty(this)
}

final case class Context (awaitTimeout: Long, maxRunIterations: Long, neuronDefaults: NeuronDefaults){
  def timeout: Timeout = Timeout(FiniteDuration.apply(awaitTimeout, TimeUnit.SECONDS))
  def threshold: Double = neuronDefaults.threshold
  def weight: SynapseTrait = neuronDefaults.weight
  def silenceIterations: Int = neuronDefaults.silenceIterations
  def iterationTime: Long = neuronDefaults.iterationTime

  private var systemOpt: Option[ActorSystem[SpawnProtocol.Command]] = None

  // A typed ActorSystem always has a user guardian and offers no actorOf for
  // top-level actors "from the outside". SpawnProtocol is Akka's built-in
  // guardian for spawning top-level actors on demand (see NetRef).
  def system: ActorSystem[SpawnProtocol.Command] = systemOpt match {
    case Some(actorSystem) => actorSystem
    case None =>
      val actorSystem = ActorSystem(SpawnProtocol(), "system")
      systemOpt = Some(actorSystem)
      actorSystem
  }

  def shutdownSystem(): Unit = if(systemOpt.isDefined) {
    systemOpt.get.terminate()
    systemOpt = None
  }

  def toJson: String = writePretty(this)
}

object Context {
  private var instance: Option[Context] = None

  def apply(): Context = {
    if(instance.isEmpty) init()
    instance.get
  }

  def set(newInstance: Context): Unit = {
    instance = Some(newInstance)
  }

  def reset(): Unit = {
    instance = None
  }

  private final def that: Context = instance.get

  def withAwaitTimeout(timeout: Long): Unit = set(apply().copy(awaitTimeout = timeout))

  def withMaxRunIterations(iterations: Long): Unit = set(apply().copy(maxRunIterations = iterations))

  def withThreshold(threshold: Double): Unit = set(apply().copy(neuronDefaults = that.neuronDefaults.copy(threshold = threshold)))

  def withWeight(weight: SynapseTrait): Unit = set(apply().copy(neuronDefaults = that.neuronDefaults.copy(weight = weight)))
  def withSilenceIterations(silenceIterations: Int): Unit =
    set(apply().copy(neuronDefaults = that.neuronDefaults.copy(silenceIterations = silenceIterations)))

  def withIterationTime(iterationTime: Long): Unit = set(apply().copy(neuronDefaults = that.neuronDefaults.copy(iterationTime = iterationTime)))

  private val _awaittimeout: String = "awaitTimeout"
  private val _maxruniterations: String = "maxRunIterations"
  private val _neurondefaults: String = "neuronDefaults"
  private val _defaultthreshold: String = "defaultThreshold"
  private val _defaultweight: String = "defaultWeight"
  private val _defaultsilenceiterations: String = "defaultSilenceIterations"
  private val _defaultiterationtime: String = "defaultIterationTime"
  
  enum Defaults (val name: String) {
    case AWAIT_TIMEOUT extends Defaults("awaitTimeout")
    case MAX_RUN_ITERATIONS extends Defaults("maxRunIterations")
    case NEURON_DEFAULTS extends Defaults("neuronDefaults")
    case DEFAULT_THRESHOLD extends Defaults("defaultThreshold")
    case DEFAULT_WEIGHT extends Defaults("defaultWeight")
    case DEFAULT_SILENCE_ITERATIONS extends Defaults("defaultSilenceIterations")
    case DEFAULT_ITERATION_TIME extends Defaults("defaultIterationTime")
  }
  import Defaults.*

  private def init(): Unit = {
    val config = ConfigFactory.load()
    val root = config.getConfig("context")

    val awaitTimeout = root.getInt(AWAIT_TIMEOUT.name)
    val maxRunIterations = root.getInt(MAX_RUN_ITERATIONS.name)

    // neuron defaults
    val neuronRoot = root.getConfig(NEURON_DEFAULTS.name)
    val threshold = neuronRoot.getDouble(DEFAULT_THRESHOLD.name)
    val weight = SynapseTrait(neuronRoot.getString(DEFAULT_WEIGHT.name))
    val silenceIterations = neuronRoot.getInt(DEFAULT_SILENCE_ITERATIONS.name)
    val iterationTime = neuronRoot.getLong(DEFAULT_ITERATION_TIME.name)

    val neuronDefaults = NeuronDefaults(threshold, weight, silenceIterations, iterationTime)

    set(Context(awaitTimeout, maxRunIterations, neuronDefaults))
  }

  def fromJson(jsonStr: String)(using ct: ClassTag[Context]): Context = read[Context](jsonStr)
  def withJson(jsonStr: String): Unit = set(fromJson(jsonStr))

  def set(action: Defaults, n: Int): Unit = action match {
    case AWAIT_TIMEOUT => withAwaitTimeout(n)
    case MAX_RUN_ITERATIONS => withMaxRunIterations(n)
    case DEFAULT_SILENCE_ITERATIONS => withSilenceIterations(n)
    case _ =>
  }
  
  def set(action: Defaults, d: Double): Unit = action match {
    case DEFAULT_THRESHOLD => withThreshold(d)
    case DEFAULT_WEIGHT => withWeight(SynapseWeight(d))
    case _ =>
  }

  def set(map: Map[Defaults, Double | Int]):Unit = map.foreach(tuple =>
    tuple._2 match {
      case d: Double => set(tuple._1, d)
      case i: Int => set(tuple._1, i)
    }
  )
}