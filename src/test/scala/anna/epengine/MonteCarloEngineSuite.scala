package anna.epengine

import anna.Context
import anna.epengine.EngineLike
import anna.async.NetBuilder
import anna.logger.LOG
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

  val simplestTemplate = NetBuilder().addInput("in").chain("dot", 1.0, 0.5)
    .use("in").chain("line", 1.0, 0.5)
    .setName("simplest").data

  val set = ExercisesSet("count fires", List("count fires 1"))

  @Test def shouldImproveExercisesResults(): Unit = {
    engine = MonteCarloEngine("montecarlo1", inputIds, outputIds, simplestTemplate, set)
    engine.calculateResults()
    var before = engine.getResult(engine.best.id).get
    for(i <- 1 to 20){
      engine.run(1)
      val after = engine.getResult(engine.best.id).get
      assertTrue(after >= before)
      before = after
    }
  }

}
