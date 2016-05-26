package anna.epengine

import anna.async.NetBuilder
import anna.async.NetBuilderOps._
import anna.data.HushValue

/**
  * Created by gorywoda on 1/31/16.
  */
case class DelayGate(name: String, delay: Int, inputTickMultiplier: Double){
  lazy val data = {
    val builder = NetBuilder()
    builder.inputTickMultiplier = inputTickMultiplier
    chain(builder)
    builder.data
  }

  def chain(builder: NetBuilder) = {
    val middleThreshold = 0.9
    val hushTime = HushValue((delay * builder.inputTickMultiplier).toInt)
    val feedbackWeight = middleThreshold / ((delay+1) * builder.inputTickMultiplier)
    if(builder.isCurrent) builder.chain(inputId, 1.0, 0.0, hushTime)
    else builder.addMiddle(id=inputId, threshold=0.0, hushValue=hushTime)

    builder.use(inputId).hush(inputId).chain(s"${name}mi", 1.0, 0.01).connect(s"${name}mi", 1.0)
           .chain(outputId, feedbackWeight, middleThreshold)
           .chainHushNeuron(hushId).hush(inputId).hush(s"${name}mi").hush(outputId)
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

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def apply(delay: Int):DelayGate = {
    val newName = nextName()
    firstFreeId += 1
    DelayGate(newName, delay, 1.0)
  }

  def apply(delay: Int, inputTickMultiplier: Double):DelayGate = {
    val newName = nextName()
    firstFreeId += 1
    DelayGate(newName, delay, inputTickMultiplier)
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