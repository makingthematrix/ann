package anna.blocks

import anna.async.NetBuilder
import anna.async.NetBuilderOps._
import anna.data.SilenceIterations

/**
  * Created by gorywoda on 6/19/16.
  */
case class SignalSum(name: String, requiredSignalsNumber: Int) extends NeuronBlock {
  import SignalSum.middleThreshold

  override def chain(builder: NetBuilder, inputWeight: Double = 1.0, inputThreshold: Double = 0.0) = {
    val middleSynapseWeigth = middleThreshold/requiredSignalsNumber

    if(builder.isCurrent) builder.chain(inputId, inputWeight, inputThreshold, 0)
    else builder.addMiddle(id=inputId, threshold=0.0, silenceIterations = SilenceIterations(0))

    builder.use(inputId).chain(outputId, middleSynapseWeigth, middleThreshold, 0)
      .addSilencingNeuron(silencingId).silence(inputId).silence(outputId)
      .use(outputId)
  }

  val inputId = SignalSum.inputId(name)
  val outputId = SignalSum.outputId(name)

  override val silencingId = SignalSum.silencingId(name)

  override val inputIds = Set(inputId)
  override val outputIds = Set(outputId)
}

object SignalSum {
  val middleThreshold = 0.9

  val blockNamePrefix = "SignalSum"
  val nameRegex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock = 3

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def apply(requiredSignalsNumber: Int):SignalSum = {
    val newName = nextName()
    firstFreeId += 1
    SignalSum(newName, requiredSignalsNumber)
  }

  def inputId(name: String) = s"${name}1"
  def outputId(name: String) = s"${name}2"
  def silencingId(name: String) = s"${name}s"

}
