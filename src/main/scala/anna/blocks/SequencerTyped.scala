package anna.blocks

import anna.async.NetBuilderTyped
import anna.async.NetBuilderOpsTyped._
import anna.async.NeuronTyped

/**
 * Typed-Actor version of [[Sequencer]].
 *
 * Same block topology and behaviour, but built through [[NetBuilderTyped]] /
 * `NetBuilderOpsTyped` (so the block is made of typed neurons/synapses).
 *
 * A sequencer fires its output only when its two inputs arrive in order
 * (input1 then input2): each of the two block neurons silences itself forever
 * after firing and wakes the other, so only the correct alternating sequence
 * lets the second neuron (the output) fire. The silencing neuron resets the
 * block to its initial state.
 *
 * Note: unlike the classic companion, there is no `apply(Int)` factory — that
 * method in `Sequencer` was a copy-paste bug returning a `SignalSum` and is
 * never used. Construct with the block name via the case class / DSL instead.
 */
final case class SequencerTyped(name: String) {
  def chain(builder: NetBuilderTyped,
            input1Id: String, input2Id: String,
            input1Weight: Double = 1.0, input1Threshold: Double = 0.0,
            input2Weight: Double = 1.0, input2Threshold: Double = 0.0
           ) =
    // oh, screw it. Let's assume input neurons are already there.
    builder.use(input1Id).chain(this.input1Id, input1Weight, input1Threshold, NeuronTyped.SilenceForever).silence(this.input1Id)
           .use(input2Id).chain(this.input2Id, input2Weight, input2Threshold, NeuronTyped.SilenceForever).initSilent().silence(this.input2Id)
           .use(this.input1Id).wake(this.input2Id)
           .use(this.input2Id).wake(this.input1Id)
           .addSilencingNeuron(silencingId).wake(this.input1Id).silence(this.input2Id)
           .use(this.input2Id)

  val input1Id = SequencerTyped.input1Id(name)
  val input2Id = SequencerTyped.input2Id(name)
  val outputId = input2Id
  val silencingId = SequencerTyped.silencingId(name)
}

object SequencerTyped {
  val blockNamePrefix = "Sequencer"
  val nameRegex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock = 3

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def input1Id(name: String) = s"${name}1"
  def input2Id(name: String) = s"${name}2"
  def outputId(name: String) = input2Id(name)
  def silencingId(name: String) = s"${name}s"
}
