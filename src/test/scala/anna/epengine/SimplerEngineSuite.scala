package anna.epengine

import anna.Context
import anna.async.{NetWrapper, NetBuilder}
import anna.data.NetData
import anna.logger.LOG
import org.junit.Assert._
import org.junit.{Test, After, Before}
import org.scalatest.junit.JUnitSuite
import anna.async.NetBuilderOps._


/**
  * Created by gorywoda on 1/8/16.
  */
class SimplerEngineSuite extends JUnitSuite {

  private var _oldContext:Context = _

  @Before def before() {
    _oldContext = Context()
    LOG.addLogToStdout()
  }

  @After def after(): Unit ={
    Context.set(_oldContext)
  }

  private var engine: SimplerEngine = null
  private lazy val inputIds = List("in1")
  private lazy val outputIds = List("out1")

  private lazy val netTemplate = {
    val builder = NetBuilder()
    builder.netId = "net"
    builder.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    builder.use("in1").chain("net1_3",1.0,0.0).chain("net1_4",1.0,0.0).connect("out1",1.0)
    builder.data
  }

  val t1 = new Exercise("any response to any signal", 1, List("out1")) {
    def run(wrapper: NetWrapper) = {
      var counter = 0
      wrapper.addAfterFire("out1")( (_:Double)=> {
        counter += 1
      })

      wrapper += "1"

      wrapper.tickUntilCalm()
      if (counter > 0) 1.0 else 0.0
    }
  }

  val t2 = new Exercise("constant output", 1, List("out1")) {
    def run(wrapper: NetWrapper):Double = {
      var counter = 0
      wrapper.addAfterFire("out1")( (_:Double)=> {
        counter += 1
      })

      wrapper += "1,1,1,1,1,1"

      wrapper.tickUntilCalm()
      if (counter == 6) 1.0 else 0.0
    }
  }

  val exercisesSet = ExercisesSet("randomset", List("random result 0-1"))


  @Test def shouldNeitherCrossNorMutate(): Unit ={
    val dirName = "test-shouldNeitherCrossNorMutate"
    engine = SimplerEngine(dirName, inputIds, outputIds, netTemplate, exercisesSet, MutationsProfile.noMutations)

    Context.withMutationProbability(0.0)
    Context.withCrossCoefficient(0.0)

    engine.run()

    // the best genome is not even cloned with the new netId - it's just taken from the old generation and put in the new one
    val bestGenome = engine.best
    val notCloned = engine.poll.genomes.filterNot(_.id.contains("Cloned"))
    assertEquals(1, notCloned.size)
    assertEquals(bestGenome, notCloned.head)
  }

  @Test def shouldCopyTheBestGenome(): Unit = {
    val specialNeuronId = "middle"
    val bestResult = 1.0
    val worseResult = 0.5

    val ex = new Exercise("shouldCloneTheBestGenome-exercise", 1, List("out1")) {
      def run(wrapper: NetWrapper): Double = wrapper.net.getNeurons.find(n => NetData.removeNetId(n.id) == specialNeuronId) match {
        case Some(n) => bestResult
        case None => worseResult
      }
    }

    Context.withMutationProbability(1.0)
    Context.withCrossCoefficient(1.0)

    val map = AccessMap(List("in1"), List("out1"))

    val g1 = NetGenome(NetBuilder("best").addInput("in1").chain("middle",1.0,1.0).chain("out1",0.5,0.5).data, map)
    val g2 = NetGenome(NetBuilder("other-1").addInput("in1").chain("out1",0.0,0.0).data, map)
    val g3 = NetGenome(NetBuilder("other-2").addInput("in1").chain("out1",1.0,1.0).data, map)

    val engine = SimplerEngine(List(ex), List(g1, g2, g3), MutationsProfile.noMutations)

    engine.calculateResults()
    assertEquals(g1.data, engine.best.data)

    engine.run()
    val bestGenome = engine.best

    assertEquals(bestResult, engine.getResult(bestGenome.id).get, 0.01)
    assertEquals("best", bestGenome.id)
    assertTrue(bestGenome.neurons.find(n => NetData.removeNetId(n.id) == specialNeuronId).isDefined)
  }

  @Test def shouldWorkOnSOS(): Unit = {
    val template = NetBuilder().SOSNetData()
    val inputs = List("in")
    val outputs = List("dot", "line", "S", "O")
    val genomePollSize = 5
    val initialMutationsNumber = 5

    val genomePoll = GenomePoll(template, inputs, outputs, genomePollSize, MutationsProfile.simpleMutations, initialMutationsNumber)
    assertEquals(genomePollSize, genomePoll.genomes.size)

    val insAndOuts = (inputs ++ outputs).toSet
    genomePoll.genomes.foreach( genome => {
      assertEquals(insAndOuts, genome.notFullAccessNeurons.map(_.id).toSet)
      assertNotEquals(template.toJson, genome.toJson)
    })

    val exSet = ExercisesSet.load("sosset")
    val coach = Coach(exSet.exercises) // @todo: why exactly Coach is different from ExercisesSet?
    val templateResult = coach.test(template)
    println(s"template result: $templateResult")
    assertEquals(171.6, templateResult, 0.1) // in current state as of 2016-01-08

    val engine = SimplerEngine(coach, genomePoll, MutationsProfile.simpleMutations)
    engine.calculateResults()

    val best0 = engine.results(engine.best.id)
    println(s"best0 result: $best0")

    engine.run(1)

    val best1 = engine.results(engine.best.id)
    println(s"best1 result: $best1")

    assertTrue(best1 >= best0)
  }

}
