package anna.epengine

import anna.logger.LOG._

/**
 * Created by gorywoda on 03.08.15.
 */
trait EngineLike {
  // these three need to be implemented
  def poll: GenomePoll
  def calculateResults(): Unit
  protected def _run(): Unit

  def best = {
    if(poll.empty) exception("The genomes poll is empty")
    poll.genomesSorted(_results)(0)
  }

  def getResult(netId: String) = _results.get(netId)

  def results = _results.toMap

  def iteration = _iteration

  def run(iterations: Int =20) = {
    val end = _iteration + iterations
    while(_iteration < end) _run()
  }

  def avg = poll.ids.map(_results.get(_).get).sum / poll.ids.size

  def median = {
    val list = results.map(_._2).toList.sorted
    list(results.size/2)
  }

  def quintiles = for(i <- 1 to 5) yield quintile(i)

  def quintile(n: Int) = {
    assert(n >= 1 && n <= 5, s"The number of a quintile must be between 1 and 5, is $n")
    val list = results.map(_._2).toList.sorted
    val t = results.size/5
    ((n-1)*t until n*t).map(list(_)).sum / t
  }

  def runWithStats(iterations: Int =20) = (1 to iterations).map(_ => {
    run()
    EvolutionStats(iteration, best.data.id, getResult(best.data.id).get, avg, poll.size, median, quintiles.toList)
  }).sortBy(_.iteration).toList

  protected var _results:Map[String,Double] = poll.genomes.map(g => g.id -> 0.0).toMap

  protected var _iteration = 0
}
