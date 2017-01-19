package anna.blocks

import anna.async.NetBuilder

/**
  * Created by gorywoda on 1/19/17.
  */
trait NeuronBlock {
  val name: String
  val inputIds: Set[String]
  val outputIds: Set[String]
  val silencingId: String

  lazy val data = {
    val builder = NetBuilder()
    chain(builder)
    builder.data
  }

  def chain(builder: NetBuilder, inputWeight: Double = 1.0, inputThreshold: Double = 0.0): NetBuilder
}
