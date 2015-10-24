package anna.epengine

import anna.Context
import anna.async.NetBuilder
import anna.logger.LOG
import anna.logger.LOG._
import anna.utils.Utils
import org.junit.{After, Before}
import org.scalatest.junit.JUnitSuite
import anna.async.NetBuilderOps._
import org.junit.Assert._
import org.junit.{After, Before, Test}

/**
 * Created by gorywoda on 05.08.15.
 */
class MonteCarloEngineSuite extends JUnitSuite {

  private var _oldContext:Context = _

  @Before def before() {
    _oldContext = Context()
    LOG.addLogToStdout()
  }

  @After def after(): Unit ={
    if(engine != null && Utils.fileExists(engine.dirPath)) Utils.deleteDir(engine.dirPath)
    Context.set(_oldContext)
  }

  private var engine: EngineLike = null

  val inputIds = List("in")
  val outputIds = List("dot", "line")
  val accessMap = NetGenome.accessMap(inputIds, outputIds)

  lazy val simplestTemplate = {
    val data = NetBuilder().addInput("in").chain("dot", 1.0, 0.5)
      .use("in").chain("line", 1.0, 0.5).data
    val genome = NetGenome(data, accessMap)
    genome.netId("simplest").data
  }

  lazy val dotLineTemplate = {
    val data = NetBuilder().addInput("in").chain("mi11",1.0,0.15).chain("mi12",1.0,0.15).chain("dot",1.0,0.15)
      .use("in").chain("mi21",1.0,0.15).chain("mi22",1.0,0.15).chain("line",1.0,0.15)
      .use("mi12").hush("mi21")
      .use("mi21").hush("mi11")
      .use("dot").hush("line")
      .use("line").hush("dot")
      .data
    val genome = NetGenome(data, accessMap)
    genome.netId("dotline").data
  }

  val set = ExercisesSet("count fires", List("count fires 1"))

  @Test def shouldImproveExercisesResults(): Unit = {
    engine = MonteCarloEngine("montecarlo1", inputIds, outputIds, simplestTemplate, set)
    engine.calculateResults()
    val first = engine.getResult(engine.best.id).get
    var best = first
    for(i <- 1 to 5){
      engine.run(1)
      val newBest = engine.getResult(engine.best.id).get
      assertTrue(newBest >= best)
      best = newBest
      debug(this, s" best result: $best")
    }
    assertTrue(best >= first)
  }

  @Test def shouldLooseWithStandardEngine(): Unit = {
    Context.withGenomePollSize(25)

    val initialMutationsNumber = Context().initialMutationsNumber

    Context.withInitialMutationsNumber(50)
    engine = MonteCarloEngine("test-shouldLooseWithStandardEngine-MCE", inputIds, outputIds, dotLineTemplate, set)
    engine.calculateResults()
    val firstMCE = engine.getResult(engine.best.id).get
    engine.run(5)
    val bestMCE = engine.getResult(engine.best.id).get
    Utils.deleteDir(engine.dirPath)

    Context.withInitialMutationsNumber(initialMutationsNumber)
    engine = StandardEngine("test-shouldLooseWithStandardEngine-SE", inputIds, outputIds, dotLineTemplate, set)
    engine.calculateResults()
    val firstSE = engine.getResult(engine.best.id).get
    engine.run(5)
    val bestSE = engine.getResult(engine.best.id).get

    debug(this, s" first MCE result: $firstMCE")
    debug(this, s" best MCE result: $bestMCE")
    debug(this, s" first SE result: $firstSE")
    debug(this, s" best SE result: $bestSE")

    assertTrue(bestSE > bestMCE)
  }

}
