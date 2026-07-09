package anna.blocks

import anna.async.NetBuilderTyped
import anna.async.NetBuilderOpsTyped._

/**
 * Typed-Actor version of [[DelayGate]].
 *
 * Same block topology and behaviour, but built through [[NetBuilderTyped]] /
 * `NetBuilderOpsTyped` (so the block is made of typed neurons/synapses).
 *
 * A delay gate fires its output exactly `delay` iterations after receiving an
 * input signal: the input neuron silences itself for `delay` iterations, a
 * self-looping middle neuron keeps re-firing while it is awake, and the output
 * neuron accumulates the middle's feedback until it crosses the threshold.
 */
final case class DelayGateTyped(name: String, delay: Int) {
  lazy val data = {
    val builder = NetBuilderTyped()
    chain(builder)
    builder.data
  }

  def chain(builder: NetBuilderTyped, inputWeight: Double = 1.0, inputThreshold: Double = 0.0) = {
    val feedbackWeight = DelayGateTyped.middleThreshold / (delay + 1)
    if (builder.isCurrent) builder.chain(inputId, inputWeight, inputThreshold, delay)
    else builder.addMiddle(id = inputId, threshold = inputThreshold, silenceIterations = delay)

    builder.use(inputId).silence(inputId).chain(middleId, 1.0, 0.01).connect(middleId, 1.0)
           .chain(outputId, feedbackWeight, DelayGateTyped.middleThreshold).silence(middleId)
           .addSilencingNeuron(silencingId).silence(inputId).silence(middleId).silence(outputId)
           .use(outputId) // always end chaining with setting the current neuron at the main output of the block
  }

  val inputId = DelayGateTyped.inputId(name)
  val middleId = DelayGateTyped.middleId(name)
  val outputId = DelayGateTyped.outputId(name)
  val silencingId = DelayGateTyped.silencingId(name)
}

object DelayGateTyped {
  val blockNamePrefix = "DelayGate"
  val nameRegex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock = 4

  val middleThreshold = 0.9

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def apply(delay: Int): DelayGateTyped = {
    val newName = nextName()
    firstFreeId += 1
    DelayGateTyped(newName, delay)
  }

  def inputId(name: String) = s"${name}1"
  def middleId(name: String) = s"${name}2"
  def outputId(name: String) = s"${name}3"
  def silencingId(name: String) = s"${name}s"
}
