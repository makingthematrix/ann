package anna.blocks

import anna.async.NetBuilder
import anna.data.{SilenceForever, SynapseWeight}

/**
  * Created by gorywoda on 1/17/17.
  */
case class Sequencer(name: String) extends NeuronBlock {

  override def chain(builder: NetBuilder, inputWeight: Double = 1.0, inputThreshold: Double = 0.0) = {
    if(builder.isCurrent) builder.chain(inputId1, SynapseWeight(inputWeight), inputThreshold, SilenceForever())
    else builder.addMiddle(id=inputId1, threshold=0.0, silenceIterations = SilenceForever())

    builder
      .addMiddle(id=inputId2, threshold=inputThreshold, silenceIterations = SilenceForever()).initSilenced(inputId2)
      .use(inputId1).speakUp(inputId2).silence(inputId1)
      .use(inputId2).speakUp(inputId1).silence(inputId2)
      .addSilencingNeuron(silencingId).speakUp(inputId1).silence(inputId2)
      .use(outputId)
  }

  val inputId1 = Sequencer.inputId1(name)
  val inputId2 = Sequencer.inputId2(name)
  val outputId = Sequencer.outputId(name)

  override val silencingId = Sequencer.silencingId(name)

  override val inputIds = Set(inputId1, inputId2)
  override val outputIds = Set(outputId)
}

object Sequencer {

  val blockNamePrefix = "Sequencer"
  val nameRegex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock = 2

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def apply():Sequencer = {
    val newName = nextName()
    firstFreeId += 1
    Sequencer(newName)
  }

  def inputId1(name: String) = s"${name}n1"
  def inputId2(name: String) = s"${name}n2"
  def outputId(name: String) = s"${name}n2"
  def silencingId(name: String) = s"${name}s"
}