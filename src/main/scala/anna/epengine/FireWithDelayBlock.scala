package anna.epengine

import anna.async.NetBuilder
import anna.async.NetBuilderOps._
import anna.data.HushValue

/**
  * Created by gorywoda on 1/31/16.
  */
case class FireWithDelayBlock(name: String, delay: Double, inputTickMultiplier: Double){
  lazy val data = {
    val builder = NetBuilder()
    builder.inputTickMultiplier = inputTickMultiplier
    chain(builder)
    builder.data
  }

  def chain(builder: NetBuilder) = {
    val coeff = delay * builder.inputTickMultiplier * 1.55 // a magic number to counteract inherent delays in sending and receiving messages
    if(builder.isCurrent) builder.chain(inputId, 1.0, 0.0, HushValue(coeff.toInt))
    else builder.addMiddle(id=inputId, threshold=0.0, hushValue=HushValue(coeff.toInt))

    builder.hush(inputId)
      .chain(s"${name}mi", 1.0, 0.01).connect(s"${name}mi", 1.0)
      .chain(outputId, 0.9/coeff, 0.9)
      .chainHushNeuron(hushId).hush(inputId).hush(s"${name}mi").hush(outputId)
  }

  val inputId = FireWithDelayBlock.inputId(name)
  val outputId = FireWithDelayBlock.outputId(name)
  val hushId = FireWithDelayBlock.hushId(name)
}

object FireWithDelayBlock {
  val blockNamePrefix = "FireWithDelay"
  val nameRegex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock = 4

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def apply(delay: Double, inputTickMultiplier: Double):FireWithDelayBlock = {
    val newName = nextName()
    firstFreeId += 1
    FireWithDelayBlock(newName, delay, inputTickMultiplier)
  }

  def inputId(name: String) = s"${name}in"
  def outputId(name: String) = s"${name}out"
  def hushId(name: String) = s"${name}hush"

  def blocksInGenome(gen: NetGenome) = gen.neurons.flatMap( _.id match {
    case nameRegex(number) => Some(number.toInt)
    case _ => None
  }).toSet[Int].map(n => s"${blockNamePrefix}#${n}#") // @todo: why toSet[Int] works, but toSet does not

}