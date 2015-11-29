package anna.epengine

import anna.{ContextDoubleRange, ContextMatrix, Context}
import anna.async.{NeuronCounter, NetBuilder}
import anna.logger.LOG
import anna.utils.Utils
import anna.utils.DoubleRange._
import org.junit.{Test, After, Before}
import org.scalatest.junit.JUnitSuite
import anna.async.NetBuilderOps._
import anna.Context._
import org.junit.Assert._

/**
 * Created by gorywoda on 17.07.15.
 */

class StressSuite extends JUnitSuite {
  private var _oldContext:Context = _

  @Before def before() {
    _oldContext = Context()
    LOG.addLogToStdout()
  }

  @After def after(): Unit ={
    if(engine != null && Utils.fileExists(engine.dirPath)) Utils.deleteDir(engine.dirPath)
    Context.set(_oldContext)
  }

  private var engine: StandardEngine = null

  private lazy val inputIds = List("in")
  private lazy val outputIds = List("dot","line")
  private lazy val accessMap = Map("in" -> MutationAccessDontMutate(),
                                   "dot" -> MutationAccessDontDelete(),
                                   "line" -> MutationAccessDontDelete())
  private lazy val exercisesSet = ExercisesSet.load("dotlineset")


  lazy val dotLineData = {
    val data = NetBuilder().addInput("in").chain("mi11",1.0,0.5).chain("mi12",1.0,0.5).chain("dot",1.0,0.5)
      .use("in").chain("mi21",1.0,0.5).chain("mi22",1.0,0.5).chain("line",1.0,0.5)
      .use("mi12").hush("mi21")
      .use("mi21").hush("mi11")
      .use("dot").hush("line")
      .use("line").hush("dot")
      .data
    val genome = NetGenome(data, accessMap)
    genome.netId("dotline").data
  }

  @Test def shouldSurvive1000Restarts(): Unit = {
    assertEquals(0, NeuronCounter.size)
    var counter = 0
    try {
      while(counter < 1000) {
        val wrapper = NetBuilder().set(dotLineData).build()
        wrapper.shutdown()
        NeuronCounter.clean()
        println(counter)
        counter += 1
      }
    } catch {
      case ex: java.lang.OutOfMemoryError =>
        LOG.error(this,s"out of memory! neuron counter size is : ${NeuronCounter.size}, counter is $counter")
        LOG.error(this,NeuronCounter.neurons.keySet.mkString(","))
        LOG.error(this,ex.getMessage)
        fail()
    }
    assertEquals(0, NeuronCounter.size)
  }

  @Test def shouldSurvive1000RestartsWithWork(): Unit = {
    assertEquals(0, NeuronCounter.size)
    var counter = 0
    try {
      while(counter < 1000) {
        val wrapper = NetBuilder().set(dotLineData).build()
        wrapper.tickUntilCalm("1")
        wrapper.shutdown()
        println(counter)
        counter += 1
      }
    } catch {
      case ex: java.lang.OutOfMemoryError =>
        LOG.error(this,s"out of memory! neuron counter size is : ${NeuronCounter.size}, counter is $counter")
        LOG.error(this,NeuronCounter.neurons.keySet.mkString(","))
        LOG.error(this,ex.getMessage)
        fail()
    }
    assertEquals(0, NeuronCounter.size)
  }

  @Test def shouldNotCrashSearchBestContext(): Unit = {
    val iterations = 20
    val cv1 = ContextDoubleRange(_mutationprobability, 0.2 <=> 0.8, 3)
    val cv2 = ContextDoubleRange(_crosscoefficient, 0.2 <=> 0.8, 3)
    val cm = ContextMatrix(List(cv1, cv2))

    // @todo: change it to smoething smarter
    val mutationsProfile = MutationsProfile.nullProfile

    cm.unfold.map(contextVector => {
      Context.set(contextVector)
      val engine = StandardEngine("engine", inputIds, outputIds, dotLineData, exercisesSet, mutationsProfile)
      val stats = engine.runWithStats(iterations)
      LOG.debug("-----------------------------------")
      LOG.debug(s"context vector: $contextVector")
      LOG.debug(s"results for the iteration $iterations :")
      LOG.debug(stats.last.toString)
    })
  }
}
