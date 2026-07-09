package anna

import java.util.concurrent.TimeUnit

import akka.actor.typed.{ActorSystem, SpawnProtocol}
import akka.util.Timeout
import anna.data._
import anna.utils.Utils.formats
import com.typesafe.config.ConfigFactory
import org.json4s.native.Serialization.{read, writePretty}

import anna.logger.LOG._

import scala.concurrent.duration._

final case class NeuronDefaults(threshold: Double, weight: SynapseTrait, silenceIterations: Int, iterationTime: Long){
  def toJson: String = writePretty(this)
}

final case class Context(awaitTimeout: Long, maxRunIterations: Long, neuronDefaults: NeuronDefaults){
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

  def fromJson(jsonStr: String): Context = read[Context](jsonStr)
  def withJson(jsonStr: String): Unit = set(fromJson(jsonStr))

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
    tuple._2 match {
      case d: Double => set(tuple._1, d)
      case i: Int => set(tuple._1, i)
      case _ => exception(this, s"Unsuppored type of ${tuple._1}: ${tuple._2.getClass}")
    }
  )
}