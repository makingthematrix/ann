package anna.epengine

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

  def chain(builder: NetBuilder) = {
    val hushTime = HushValue(delay)
    val feedbackWeight = DelayGate.middleThreshold / (delay + 1)
    if(builder.isCurrent) builder.chain(inputId, 1.0, 0.0, hushTime)
    else builder.addMiddle(id=inputId, threshold=0.0, hushValue=hushTime)

    val middleId = s"${name}mi"

    builder.use(inputId).hush(inputId).chain(middleId, 1.0, 0.01).connect(middleId, 1.0)
           .chain(outputId, feedbackWeight, DelayGate.middleThreshold).hush(middleId)
           .addHushNeuron(hushId).hush(inputId).hush(middleId).hush(outputId)
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

  def blocksInGenome(gen: NetGenome) = gen.neurons.flatMap( _.id match {
    case nameRegex(number) => Some(number.toInt)
    case _ => None
  }).toSet[Int].map(n => s"${blockNamePrefix}#${n}#") // @todo: why toSet[Int] works, but toSet does not

}