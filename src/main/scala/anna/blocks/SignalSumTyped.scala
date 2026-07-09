package anna.blocks

import anna.async.NetBuilderTyped
import anna.async.NetBuilderOpsTyped._

/**
 * Typed-Actor version of [[SignalSum]].
 *
 * Same block topology and behaviour, but built through [[NetBuilderTyped]] /
 * `NetBuilderOpsTyped` (so the block is made of typed neurons/synapses).
 *
 * A signal-sum block fires its output once it has received `requiredSignalsNumber`
 * input signals within a single (uninterrupted) run: each input adds a fraction
 * of the output threshold, and the block silences itself after firing.
 */
final case class SignalSumTyped(name: String, requiredSignalsNumber: Int) {
  import SignalSumTyped.middleThreshold

  lazy val data = {
    val builder = NetBuilderTyped()
    chain(builder)
    builder.data
  }

  def chain(builder: NetBuilderTyped, inputWeight: Double = 1.0, inputThreshold: Double = 0.0) = {
    val middleSynapseWeigth = middleThreshold / requiredSignalsNumber

    if (builder.isCurrent) builder.chain(inputId, inputWeight, inputThreshold, 0)
    else builder.addMiddle(id = inputId, threshold = 0.0, silenceIterations = 0)

    builder.use(inputId).chain(outputId, middleSynapseWeigth, middleThreshold, 0)
      .addSilencingNeuron(silencingId).silence(inputId).silence(outputId)
      .use(outputId)
  }

  val inputId = SignalSumTyped.inputId(name)
  val outputId = SignalSumTyped.outputId(name)
  val silencingId = SignalSumTyped.silencingId(name)
}

object SignalSumTyped {
  val middleThreshold = 0.9

  val blockNamePrefix = "SignalSum"
  val nameRegex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock = 3

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def apply(requiredSignalsNumber: Int): SignalSumTyped = {
    val newName = nextName()
    firstFreeId += 1
    SignalSumTyped(newName, requiredSignalsNumber)
  }

  def inputId(name: String) = s"${name}1"
  def outputId(name: String) = s"${name}2"
  def silencingId(name: String) = s"${name}s"
}
