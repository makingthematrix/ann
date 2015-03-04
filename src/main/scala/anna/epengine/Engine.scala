package anna.epengine

import scala.collection.immutable.HashMap

class Engine(val poll: GenomePoll, val tester: Tester, private var results:Map[String,Double] = HashMap()) {
  private var initialized = false

  def run() = {
    if(!initialized){
      calculateResults()
      initialized = true
    }

    val sum = results.values.sum
    val resultsNormalized = results.map( tuple => (tuple._1 -> tuple._2/sum) ).toList.sortBy(-_._2).toMap

    // single out one genome from the lower half and cross it with one genome from the higher half
    
  }

  def best = poll.genomes.find( _.id == results.maxBy(_._2)._1 ).get

  private def calculateResults(): Unit ={
    results = tester.test(poll).map( tuple => (tuple._1.id -> tuple._2) ).toMap
  }
}

object Engine {
  def apply(poll: GenomePoll, tester: Tester) = new Engine(poll, tester, poll.genomes.map(g => (g.id -> 0.0)).toMap)

}