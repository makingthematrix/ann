package anna.epengine

import scala.collection.mutable

/**
  * Created by gorywoda on 12/27/15.
  */
abstract class Crosser(val poll: GenomePoll, val results:Map[String, Double]) {
  def newGeneration(iteration: Int):List[NetGenome]
}

trait CrosserBuilder {
  def build(poll: GenomePoll, results: Map[String, Double]):Crosser
}

object Crosser {
  private val crosserBuilders = mutable.Map[String, CrosserBuilder]()

  def apply(crosserId: String, poll: GenomePoll, results: Map[String, Double]) = crosserBuilders.get(crosserId) match {
    case Some(builder) => builder.build(poll, results)
    case None => throw new IllegalArgumentException(s"There is no registered crosser builder with id $crosserId")
  }

  def register(crosserId: String, builder: CrosserBuilder) = crosserBuilders.get(crosserId) match {
    case Some(builder) =>  throw new IllegalArgumentException(s"There is an already registered crosser builder with id $crosserId")
    case None => crosserBuilders += (crosserId -> builder)
  }
}
