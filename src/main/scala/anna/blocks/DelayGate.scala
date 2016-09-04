package anna.blocks

import anna.async.NetBuilder
import anna.async.NetBuilderOps._
import anna.data.HushValue

/**
  * Created by gorywoda on 1/31/16.
  */
case class DelayGate(name: String, delay: Int){
  lazy val data = {
    val builder = NetBuilder()
    chain(builder)
    builder.data
  }

  def chain(builder: NetBuilder, inputWeight: Double = 1.0, inputThreshold: Double = 0.0) = {
    val hushTime = HushValue(delay)
    val feedbackWeight = DelayGate.middleThreshold / (delay + 1)
    if(builder.isCurrent) builder.chain(inputId, inputWeight, inputThreshold, hushTime)
    else builder.addMiddle(id=inputId, threshold=inputThreshold, hushValue=hushTime)

    builder.use(inputId).hush(inputId).chain(middleId, 1.0, 0.01).connect(middleId, 1.0).friend(inputId)
           .chain(outputId, feedbackWeight, DelayGate.middleThreshold).hush(middleId)
           .addHushNeuron(hushId).hush(inputId).hush(middleId).hush(outputId)
           .use(outputId) // always end chaining with setting the current neuron at the main output of the block
  }

  val inputId = DelayGate.inputId(name)
  val middleId = DelayGate.middleId(name)
  val outputId = DelayGate.outputId(name)
  val hushId = DelayGate.hushId(name)
}

object DelayGate {
  val blockNamePrefix = "DelayGate"
  val nameRegex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock = 4

  val middleThreshold = 0.9

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def apply(delay: Int):DelayGate = {
    val newName = nextName()
    firstFreeId += 1
    DelayGate(newName, delay)
  }

  def inputId(name: String) = s"${name}in"
  def middleId(name: String) = s"${name}mi"
  def outputId(name: String) = s"${name}out"
  def hushId(name: String) = s"${name}hush"

}