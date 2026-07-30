package anna.blocks

import anna.async.NetBuilder
import anna.async.Neuron

import scala.util.matching.Regex

/**
 * A reusable network block, assembled through [[NetBuilder]] / `NetBuilderOps`.
 *
 * A sequencer fires its output only when its two inputs arrive in order
 * (input1 then input2): each of the two block neurons silences itself forever
 * after firing and wakes the other, so only the correct alternating sequence
 * lets the second neuron (the output) fire. The silencing neuron resets the
 * block to its initial state.
 *
 * Construct with the block name via the case class or the `sequencer` DSL.
 */
final case class Sequencer(name: String) {
  import anna.data.SynapseData.given
  import scala.language.implicitConversions

  def chain(builder: NetBuilder,
            input1Id: String, input2Id: String,
            input1Weight: Double = 1.0, input1Threshold: Double = 0.0,
            input2Weight: Double = 1.0, input2Threshold: Double = 0.0
           ): NetBuilder =
    // oh, screw it. Let's assume input neurons are already there.
    builder.use(input1Id).chain(this.input1Id, input1Weight, input1Threshold, Neuron.SilenceForever).silence(this.input1Id)
           .use(input2Id).chain(this.input2Id, input2Weight, input2Threshold, Neuron.SilenceForever).initSilent().silence(this.input2Id)
           .use(this.input1Id).wake(this.input2Id)
           .use(this.input2Id).wake(this.input1Id)
           .addSilencingNeuron(silencingId).wake(this.input1Id).silence(this.input2Id)
           .use(this.input2Id)

  val input1Id: String = Sequencer.input1Id(name)
  val input2Id: String = Sequencer.input2Id(name)
  val outputId: String = input2Id
  val silencingId: String = Sequencer.silencingId(name)
}

object Sequencer {
  val blockNamePrefix: String = "Sequencer"
  val nameRegex: Regex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock: Int = 3

  def nextName(): String = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId: Int = 1

  def input1Id(name: String): String = s"${name}1"
  def input2Id(name: String): String = s"${name}2"
  def outputId(name: String): String = input2Id(name)
  def silencingId(name: String): String = s"${name}s"
}
