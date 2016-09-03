package anna.epengine

import anna.async.NetBuilder
import anna.async.NetBuilderOps._
import anna.data.HushValue

/**
  * Created by gorywoda on 7/10/16.
  */
case class DelayGateWithBreak(name: String, delay: Int){
  import DelayGateWithBreak.middleThreshold

  lazy val data = {
    val builder = NetBuilder()
    chain(builder)
    builder.data
  }

  def chain(builder: NetBuilder, inputWeight: Double = 1.0, inputThreshold: Double = 0.0) = {
    val hushTime = HushValue(delay - 1)
    val feedbackWeight = middleThreshold / (delay + 1)
    if(builder.isCurrent) builder.chain(inputId, inputWeight, inputThreshold, hushTime)
    else builder.addMiddle(id=inputId, threshold=inputThreshold, hushValue=hushTime)

    val middleId = s"${name}mi"

    builder.use(inputId).delayGate(DelayGateWithBreak.innerDGName(name), delay, 1.0, 0.0)
      .use(inputId).chain(breakId, middleThreshold / 2.0, middleThreshold, HushValue(0)).hush(inputId).hush(innerDGHushId)
      .addHushNeuron(hushId).hush(inputId).hush(innerDGHushId).hush(breakId)
      .use(outputId).hush(breakId)
  }

  val inputId = DelayGateWithBreak.inputId(name)
  val outputId = DelayGateWithBreak.outputId(name)
  val hushId = DelayGateWithBreak.hushId(name)
  val breakId = DelayGateWithBreak.breakId(name)
  val innerDGHushId = DelayGate.hushId(DelayGateWithBreak.innerDGName(name))
}

object DelayGateWithBreak {
  val blockNamePrefix = "DelayGateWithBreak"
  val nameRegex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock = 3 + DelayGate.neuronsInBlock

  val middleThreshold = 0.9

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def apply(delay: Int):DelayGateWithBreak = {
    val newName = nextName()
    firstFreeId += 1
    DelayGateWithBreak(newName, delay)
  }

  def inputId(name: String) = s"${name}in"
  def middleId(name: String) = innerDGName(name) + "mi"
  def outputId(name: String) = innerDGName(name) + "out"
  def hushId(name: String) = s"${name}hush"
  def breakId(name: String) = s"${name}break"

  def innerDGName(name: String) = name + "innerDG"

  def blocksInGenome(gen: NetGenome) = gen.neurons.flatMap( _.id match {
    case nameRegex(number) => Some(number.toInt)
    case _ => None
  }).toSet[Int].map(n => s"${blockNamePrefix}#${n}#") // @todo: why toSet[Int] works, but toSet does not
}
