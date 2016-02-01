package anna.data

import anna.async.NetBuilder
import anna.async.NetBuilderOps._

/**
  * Created by gorywoda on 1/31/16.
  */
case class FireWithDelayBlock(name: String, delay: Double, inputTickMultiplier: Double = 3.0, defSlope: Double = 5.0){
  lazy val data = {
    val builder = NetBuilder()
    builder.inputTickMultiplier = inputTickMultiplier
    builder.defSlope = defSlope
    chain(builder)
    builder.data
  }

  def chain(builder: NetBuilder) = {
    val coeff = delay * builder.inputTickMultiplier * 1.55 // a magic number to counteract inherent delays in sending and receiving messages
    builder.chain(inputId, 1.0, 0.0, HushValue(coeff.toInt)).hush(inputId)
      .chain(s"${name}_mi", 1.0, 0.01).connect(s"${name}_mi", 1.0)
      .chain(outputId, 0.9/coeff, 0.9)
      .chainHushNeuron(hushId).hush(inputId).hush(s"${name}_mi").hush(outputId)
  }

  val inputId = s"${name}_in"
  val outputId = s"${name}_out"
  val hushId = s"${name}_hush"
}
