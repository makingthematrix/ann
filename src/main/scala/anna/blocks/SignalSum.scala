package anna.blocks

import anna.async.NetBuilder
import anna.async.NetBuilderOps._
import anna.data.HushValue
import anna.epengine.NetGenome

/**
  * Created by gorywoda on 6/19/16.
  */
case class SignalSum(name: String, requiredSignalsNumber: Int){
  import SignalSum.middleThreshold

  lazy val data = {
    val builder = NetBuilder()
    chain(builder)
    builder.data
  }

  def chain(builder: NetBuilder, inputWeight: Double = 1.0, inputThreshold: Double = 0.0) = {
    val middleSynapseWeigth = middleThreshold/requiredSignalsNumber

    if(builder.isCurrent) builder.chain(inputId, inputWeight, inputThreshold, HushValue(0))
    else builder.addMiddle(id=inputId, threshold=0.0, hushValue = HushValue(0))

    builder.use(inputId).chain(outputId, middleSynapseWeigth, middleThreshold, HushValue(0))
      .addHushNeuron(hushId).hush(inputId).hush(outputId)
      .use(outputId)
  }

  val inputId = SignalSum.inputId(name)
  val outputId = SignalSum.outputId(name)
  val hushId = SignalSum.hushId(name)
}

object SignalSum {
  val middleThreshold = 0.9

  val blockNamePrefix = "SignalSum"
  val nameRegex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock = 3

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def apply(requiredSignalsNumber: Int):SignalSum = {
    val newName = nextName()
    firstFreeId += 1
    SignalSum(newName, requiredSignalsNumber)
  }

  def inputId(name: String) = s"${name}in"
  def outputId(name: String) = s"${name}out"
  def hushId(name: String) = s"${name}hush"

  def blocksInGenome(gen: NetGenome) = gen.neurons.flatMap( _.id match {
    case nameRegex(number) => Some(number.toInt)
    case _ => None
  }).toSet[Int].map(n => s"${blockNamePrefix}#${n}#") // @todo: why toSet[Int] works, but toSet does not
}
