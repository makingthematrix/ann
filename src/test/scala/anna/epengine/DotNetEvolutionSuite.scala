package anna.epengine

import anna.logger.LOG
import anna.logger.LOG._
import org.junit.Assert._
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite

/**
 * Created by gorywoda on 06.04.15.
 */
class DotNetEvolutionSuite extends JUnitSuite {
  @Before def before() {
    LOG.addLogToStdout()
    LOG.addLogToFile(s"DotNetEvolutionSuite-${System.currentTimeMillis()}.log")
  }

  // @todo: put here something smarter ;)
  private var mutationsProfile = MutationsProfile.nullProfile

  val dotSet = ExercisesSet("dotset", List(
    "one signal gives dot",
    "one signal gives exactly one dot",
    "two signals give nothing",
    "one signal with noise gives dot",
    "one signal with noise gives exactly one dot",
    "two signals with noise give nothing",
    "one varied signal gives dot",
    "one varied signal gives exactly one dot",
    "two varied signals give nothing",
    "one varied signal with noise gives dot",
    "one varied signal with noise gives exactly one dot",
    "two varied signals with noise give nothing"
  ))

  @Test def shouldPerformEvolutionWithDotSet(): Unit ={
    val pollSize = 25

    val inputIds = List("in")
    val outputIds = List("dot")

    debug(this, s" --- creating a poll with $pollSize net genomes ---")

    val poll = GenomePoll("net", inputIds, outputIds, pollSize)

    debug(this, " --- creating the coach ---")

    val coach = Coach(dotSet)

    debug(this, " --- creating the engine ---")

    val engine = StandardEngine(coach, poll, mutationsProfile)

    debug(this, " --- calculating initial results ---")

    engine.calculateResults()

    val best1 = engine.best
    val result1 = engine.getResult(best1.id).get
    debug(this, s"the best result is $result1 of ${best1.id}")

    debug(this, " --- running the engine --- ")

    engine.run(10)

    debug(this, " --- done running the engine --- ")

    val best2 = engine.best
    val result2 = engine.getResult(best2.id).get

    assertTrue(result2 >= result1)
    println(s"new result: $result2, old result: $result1")
  }
}
