package anna.blocks

import anna.async.NetBuilder
import anna.data.NetData

import scala.util.matching.Regex

/**
 * A reusable network block, assembled through [[NetBuilder]] / `NetBuilderOps`.
 *
 * A signal-sum block fires its output once it has received `requiredSignalsNumber`
 * input signals within a single (uninterrupted) run: each input adds a fraction
 * of the output threshold, and the block silences itself after firing.
 */
final case class SignalSum(name: String, requiredSignalsNumber: Int) {
  import anna.data.SynapseData.given
  import scala.language.implicitConversions
  
  import SignalSum.middleThreshold

  lazy val data: NetData = {
    val builder = NetBuilder()
    chain(builder)
    builder.data
  }

  def chain(builder: NetBuilder, inputWeight: Double = 1.0, inputThreshold: Double = 0.0): NetBuilder = {
    val middleSynapseWeigth = middleThreshold / requiredSignalsNumber

    if (builder.isCurrent) builder.chain(inputId, inputWeight, inputThreshold, 0)
    else builder.addMiddle(id = inputId, threshold = 0.0, silenceIterations = 0)

    builder.use(inputId).chain(outputId, middleSynapseWeigth, middleThreshold, 0)
      .addSilencingNeuron(silencingId).silence(inputId).silence(outputId)
      .use(outputId)
  }

  val inputId: String = SignalSum.inputId(name)
  val outputId: String = SignalSum.outputId(name)
  val silencingId: String = SignalSum.silencingId(name)
}

object SignalSum {
  val middleThreshold: Double = 0.9

  val blockNamePrefix: String = "SignalSum"
  val nameRegex: Regex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock: Int = 3

  def nextName(): String = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId: Int = 1

  def apply(requiredSignalsNumber: Int): SignalSum = {
    val newName = nextName()
    firstFreeId += 1
    SignalSum(newName, requiredSignalsNumber)
  }

  def inputId(name: String): String = s"${name}1"
  def outputId(name: String): String = s"${name}2"
  def silencingId(name: String): String = s"${name}s"
}
