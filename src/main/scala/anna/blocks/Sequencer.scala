package anna.blocks

import anna.async.NetBuilder
import anna.async.NetBuilderOps._
import anna.data.{SilenceForever, SynapseWeight}

/**
  * Created by gorywoda on 1/17/17.
  */
case class Sequencer(name: String) {

  def chain(builder: NetBuilder, inputWeight: Double = 1.0, inputThreshold: Double = 0.0) = {
    if(builder.isCurrent) builder.chain(inputId1, SynapseWeight(inputWeight), inputThreshold, SilenceForever())
    else builder.addMiddle(id=inputId1, threshold=0.0, silenceIterations = SilenceForever())

    builder
      .use(inputId1).chain(outputId, 0.0, 0.0, 0)
      .addMiddle(id=inputId2, threshold=0.0, silenceIterations = SilenceForever()).initSilenced(inputId2)
      .use(inputId1).speakUp(inputId2).silence(inputId1)
      .use(inputId2).silence(inputId1).connect(outputId, 1.0)
      .addSilencingNeuron(silencingId).speakUp(inputId1).silence(inputId2).silence(outputId)
      .use(outputId)
  }

  val inputId1 = Sequencer.inputId1(name)
  val inputId2 = Sequencer.inputId2(name)
  val outputId = Sequencer.outputId(name)
  val silencingId = Sequencer.silencingId(name)
}

object Sequencer {

  val blockNamePrefix = "Sequencer"
  val nameRegex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock = 3

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def apply():Sequencer = {
    val newName = nextName()
    firstFreeId += 1
    Sequencer(newName)
  }

  def inputId1(name: String) = s"${name}in1"
  def inputId2(name: String) = s"${name}in2"
  def outputId(name: String) = s"${name}out"
  def silencingId(name: String) = s"${name}s"
}