package anna.blocks

import anna.async.{NetBuilder, Neuron}
import anna.async.NetBuilderOps._

case class Sequencer(name: String) {
  def chain(builder: NetBuilder,
            input1Id: String, input2Id: String,
            input1Weight: Double = 1.0, input1Threshold: Double = 0.0,
            input2Weight: Double = 1.0, input2Threshold: Double = 0.0
           ) =
    // oh, screw it. Let's assume input neurons are already there.
    builder.use(input1Id).chain(this.input1Id, input1Weight, input1Threshold, Neuron.SilenceForever).silence(this.input1Id)
           .use(input2Id).chain(this.input2Id, input2Weight, input2Threshold, Neuron.SilenceForever).initSilent().silence(this.input2Id)
           .use(this.input1Id).wake(this.input2Id)
           .use(this.input2Id).wake(this.input1Id)
           .addSilencingNeuron(silencingId).wake(this.input1Id).silence(this.input2Id)
           .use(this.input2Id)

  val input1Id = Sequencer.input1Id(name)
  val input2Id = Sequencer.input2Id(name)
  val outputId = input2Id
  val silencingId = Sequencer.silencingId(name)
}

object Sequencer {
  val blockNamePrefix = "Sequencer"
  val nameRegex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock = 3

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def apply(requiredSignalsNumber: Int):SignalSum = {
    val newName = nextName()
    firstFreeId += 1
    SignalSum(newName, requiredSignalsNumber)
  }

  def input1Id(name: String) = s"${name}1"
  def input2Id(name: String) = s"${name}2"
  def outputId(name: String) = input2Id(name)
  def silencingId(name: String) = s"${name}s"
}