package anna.blocks

/**
  * Created by gorywoda on 9/3/16.
  */
import anna.async.NetBuilder
import anna.async.NetBuilderOps._
import anna.data.{ForgetValue, HushValue}
import anna.epengine.NetGenome

case class ConstantCurrent(name: String, requiredSignalsNumber: Int){
  import ConstantCurrent.middleThreshold

  lazy val data = {
    val builder = NetBuilder()
    chain(builder)
    builder.data
  }

  def chain(builder: NetBuilder, inputWeight: Double = 1.0, inputThreshold: Double = 0.0) = {
    val middleSynapseWeigth = middleThreshold
    val forgetValue = ForgetValue(middleThreshold*(1.0-1.0/requiredSignalsNumber))

    if(builder.isCurrent) builder.chain(inputId, inputWeight, inputThreshold, HushValue(0))
    else builder.addMiddle(id=inputId, threshold=0.0, hushValue = HushValue(0))

    builder.use(inputId)
      .chain(outputId, middleSynapseWeigth, middleThreshold, HushValue(0), forgetValue)
      .addHushNeuron(hushId).hush(inputId).hush(outputId)
      .use(outputId)
  }

  val inputId = ConstantCurrent.inputId(name)
  val outputId = ConstantCurrent.outputId(name)
  val hushId = ConstantCurrent.hushId(name)
}

object ConstantCurrent {
  val middleThreshold = 0.9

  val blockNamePrefix = "ConstantCurrent"
  val nameRegex = s""".*${blockNamePrefix}#([0-9]+)#.*""".r
  val neuronsInBlock = 3

  def nextName() = s"${blockNamePrefix}#${firstFreeId}#"

  private var firstFreeId = 1

  def apply(requiredSignalsNumber: Int):ConstantCurrent = {
    val newName = nextName()
    firstFreeId += 1
    ConstantCurrent(newName, requiredSignalsNumber)
  }

  def inputId(name: String) = s"${name}in"
  def outputId(name: String) = s"${name}out"
  def hushId(name: String) = s"${name}hush"

  def blocksInGenome(gen: NetGenome) = gen.neurons.flatMap( _.id match {
    case nameRegex(number) => Some(number.toInt)
    case _ => None
  }).toSet[Int].map(n => s"${blockNamePrefix}#${n}#")
}

