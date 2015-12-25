package anna.epengine

import java.io.File

import anna.async.NetBuilderOps._
import anna.async.{NetBuilder, NetWrapper}
import anna.data.{NetData, Hush, SynapseWeight}
import anna.epengine._
import anna.logger.LOG
import anna.utils.Utils
import anna.{Commands, Context}
import org.junit.Assert._
import org.junit.{After, Before, Test}
import org.scalatest.junit.JUnitSuite


// @todo: check what it does: org.scalatest.tools.Runner$.doRunRunRunDaDoRunRun

class StandardEngineSuite extends JUnitSuite {

  private var _oldContext:Context = _

  @Before def before() {
    _oldContext = Context()
    LOG.addLogToStdout()
  }

  @After def after(): Unit ={
    Context.set(_oldContext)
  }

  private var engine: StandardEngine = null
  // @todo: put here something smarter ;)
  private var mutationsProfile = MutationsProfile.noMutations

  private lazy val inputIds = List("in1")
  private lazy val outputIds = List("out1")

  private lazy val netTemplate = {
    val builder = NetBuilder()
    builder.netName = "net"
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

  @Test def shouldTestGenomePoll(): Unit ={
    val poll = GenomePoll(netTemplate, inputIds, outputIds, 3)
    assertEquals(3, poll.genomes.size)

    val results = Coach(List(t1)).test(poll)
    results.foreach( tuple => println(s"${tuple._1.id}: ${tuple._2}"))
  }

  @Test def shouldPerformEvolutionIteration(): Unit ={
    val poll = GenomePoll(netTemplate, inputIds, outputIds, 3)
    val coach = Coach(List(t1, t2))

    engine = StandardEngine(coach, poll, mutationsProfile)
    val best1 = engine.best
    val result1 = coach.test(best1.data)

    engine.run()
  }


  @Test def shouldUseTemplateForPoll(): Unit ={
    assertNotEquals(0, Context().initialMutationsNumber)

    val inputIds = List("in1")
    val outputIds = List("out1")

    val builder = NetBuilder()
    builder.netName = "net"
    builder.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    builder.use("in1").chain("net1_3",1.0,0.0).chain("net1_4",1.0,0.0).connect("out1",1.0)

    val template = builder.data

    val poll = GenomePoll(template, inputIds, outputIds, 3, mutationsProfile)
    val d0 = poll(0).data
    val d1 = poll(1).data
    val d2 = poll(2).data

    assertNotEquals(d0, d1)
    assertNotEquals(d1, d2)
    assertNotEquals(d0, d2)
    assertNotEquals(template, d0)
    assertNotEquals(template, d1)
    assertNotEquals(template, d2)

    val set = ExercisesSet("randomset", List(
      "random result 0-1",
      "random result 0-1",
      "random result 0-1"
    ))

    val coach = Coach(set)

    engine = StandardEngine(coach, poll, mutationsProfile)
    engine.calculateResults()
    val best1 = engine.best
    val result1 = coach.test(best1.data)

    engine.run()
    assertEquals(poll.size, engine.poll.size)

    val best2 = engine.best
    val result2 = coach.test(best2.data)

    val newPoll = engine.poll
    val n0 = newPoll(0).data
    val n1 = newPoll(1).data
    val n2 = newPoll(2).data

    assertNotEquals(n0, d0)
    assertNotEquals(n0, d1)
    assertNotEquals(n0, d2)
    assertNotEquals(n1, d0)
    assertNotEquals(n1, d1)
    assertNotEquals(n1, d2)
    assertNotEquals(n2, d0)
    assertNotEquals(n2, d1)
    assertNotEquals(n2, d2)

  }

  @Test def shouldSaveAndRestoreContextFromJson(): Unit = {
    val json = Context().toJson
    println(json)

    val origWeight = Context().weight
    origWeight match {
      case Hush() => Context.withWeight(SynapseWeight(1.0))
      case SynapseWeight(_) => Context.withWeight(Hush())
    }
    assertFalse(origWeight == Context().weight)

    Context.withJson(json)
    assertTrue(origWeight == Context().weight)
  }

  @Test def shouldNeitherCrossNorMutate(): Unit ={
    val dirName = "test-shouldNeitherCrossNorMutate"
    engine = StandardEngine(dirName, inputIds, outputIds, netTemplate, exercisesSet, mutationsProfile)

    Context.withMutationProbability(0.0)
    Context.withCrossCoefficient(0.0)

    engine.run()

    assertTrue(engine.poll.genomes.filterNot(_.id.contains("Cloned")).isEmpty)
  }

  @Test def shouldCloneTheBestGenome(): Unit = {
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

    val engine = StandardEngine(Coach(List(ex)), GenomePoll(List(g1, g2, g3)), mutationsProfile)

    engine.calculateResults()
    assertEquals(g1.data, engine.best.data)

    engine.run()
    val bestGenome = engine.best

    assertEquals(bestResult, engine.getResult(bestGenome.id).get, 0.01)
    assertTrue(bestGenome.id.contains("Cloned-Best"))
    assertTrue(bestGenome.neurons.find(n => NetData.removeNetId(n.id) == specialNeuronId).isDefined)
  }
/*
  @Test def shouldLogEvolutionAndSaveResults(): Unit ={
    // tworzę engine z podaną nazwą procesu
    // tworzę, lub otwieram katalog o danej nazwie
    // 1. jeśli tworzę:
    // - podaję listę inputIds i outputIds
    // - podaję kontekst
    // - podaję szablon inicjalizacyjny
    // - podaję exercises set
    // - na podstawie inputIds, outputIds, Context i szablonu tworzę poll
    // - zapisuję poll jako iteration 0
    // 2. jeśli otwieram istniejący:
    // - wczytuję kontekst z jsona
    // - wczytuję excercises set z jsona
    // - odnajduję ostatnią iterację i wczytuję ją, tworząc poll


    // 3. podstawie exercises set tworzę coacha

    // 4. puszczam jedną iterację
    // - otwieram (lub tworzę) log iteracji z katalogu i dopisuję do niego logi działania engine
    // - otwieram (lub tworzę) log mutacji z katalogu i dopisuję do niego jakie mutacje zostały przeprowadzone na jakim genomie

    // 5. po zakończeniu iteracji
    // - zapisuję poll i listę wyników jako iterację N (będzie ją można wczytać i od niej rozpocząć kolejną iterację)
    // - w osobnym pliku zapisuję najlepszy genom

  }
  */
}