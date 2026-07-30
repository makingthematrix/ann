package anna.blocks

import anna.async.NetBuilder
import anna.data.NetData

import scala.util.matching.Regex

/**
 * A reusable network block, assembled through [[NetBuilder]] / `NetBuilderOps`.
 *
 * A delay gate fires its output exactly `delay` iterations after receiving an
 * input signal: the input neuron silences itself for `delay` iterations, a
 * self-looping middle neuron keeps re-firing while it is awake, and the output
 * neuron accumulates the middle's feedback until it crosses the threshold.
 */
final case class DelayGate(name: String, delay: Int) {
  import anna.async.NetBuilderOps.given
  import anna.data.SynapseData.given
  import scala.language.implicitConversions
  
  lazy val data: NetData = {
    val builder = NetBuilder()
    chain(builder)
    builder.data
  }

  def chain(builder: NetBuilder, inputWeight: Double = 1.0, inputThreshold: Double = 0.0): NetBuilder = {
    val feedbackWeight = DelayGate.middleThreshold / (delay + 1)
    if (builder.isCurrent) builder.chain(inputId, inputWeight, inputThreshold, delay)
    else builder.addMiddle(id = inputId, threshold = inputThreshold, silenceIterations = delay)

    builder.use(inputId).silence(inputId).chain(middleId, 1.0, 0.01).connect(middleId, 1.0)
           .chain(outputId, feedbackWeight, DelayGate.middleThreshold).silence(middleId)
           .addSilencingNeuron(silencingId).silence(inputId).silence(middleId).silence(outputId)
           .use(outputId) // always end chaining with setting the current neuron at the main output of the block
  }

  val inputId: String = DelayGate.inputId(name)
  val middleId: String = DelayGate.middleId(name)
  val outputId: String = DelayGate.outputId(name)
  val silencingId: String = DelayGate.silencingId(name)
}

object DelayGate {
  val blockNamePrefix: String = "DelayGate"
  val nameRegex: Regex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock: Int = 4

  val middleThreshold: Double = 0.9

  def nextName(): String = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId: Int = 1

  def apply(delay: Int): DelayGate = {
    val newName = nextName()
    firstFreeId += 1
    DelayGate(newName, delay)
  }

  def inputId(name: String): String = s"${name}1"
  def middleId(name: String): String = s"${name}2"
  def outputId(name: String): String = s"${name}3"
  def silencingId(name: String): String = s"${name}s"
}
