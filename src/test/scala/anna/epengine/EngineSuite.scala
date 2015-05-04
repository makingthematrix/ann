package test.async.epengine

import java.io.{IOException, File}

import anna.Context
import anna.async.{NetBuilder, NetWrapper}
import anna.epengine._
import anna.logger.LOG
import org.junit.Assert._
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite
import anna.async.NetBuilderOps._


class EngineSuite extends JUnitSuite {
  @Before def before() {
    LOG.addLogToStdout()
  }

  private lazy val inputIds = List("in1")
  private lazy val outputIds = List("out1")

  private lazy val netTemplate = {
    val builder = NetBuilder()
    builder.netName = "net"
    builder.addInput("in1").chain("net1_1",1.0,0.0).chain("net1_2",1.0,0.0).chain("out1",0.5,0.81)
    builder.use("in1").chain("net1_3",1.0,0.0).chain("net1_4",1.0,0.0).connect("out1",1.0)
    builder.data
  }
/*
  val anySignalAnyResponse = (wrapper: NetWrapper, good: Double, bad: Double) => {
    var counter = 0
    wrapper.addAfterFire("out1"){ counter += 1 }

    wrapper += "1"

    wrapper.tickUntilCalm()
    if(counter > 0) good else bad
  }

  val t1 = Exercise("any response to any signal", 1, List("out1"), anySignalAnyResponse)

  val f = (wrapper: NetWrapper, success: Double, failure: Double) => {
    var counter = 0
    wrapper.addAfterFire("out1"){ counter += 1 }

    wrapper += "1,1,1,1,1,1"

    wrapper.tickUntilCalm()
    if(counter == 6) success else failure
  }

  val t2 = Exercise("constant output", 1, List("out1"), f)

  @Test def shouldTestGenomePoll(): Unit ={
    val poll = GenomePoll("net", inputIds, outputIds, 3)
    assertEquals(3, poll.genomes.size)

    val results = Coach(List(t1)).test(poll)
    results.foreach( tuple => println(s"${tuple._1.id}: ${tuple._2}"))
  }

  @Test def shouldPerformEvolutionIteration(): Unit ={
    val poll = GenomePoll("net", inputIds, outputIds, 3)
    val tester = Coach(List(t1, t2))

    val engine = Engine(tester, 0.5, poll)
    val best1 = engine.best
    val result1 = tester.test(best1.data)

    engine.run()

    val best2 = engine.best
    val result2 = tester.test(best2.data)

    assertTrue(result2 >= result1)
    println(s"new result: $result2, old result: $result1")
  }

  @Test def shouldPerformIterationWithFakeResults(): Unit = {
    val inputIds = List("in")
    val outputIds = List("out1")

    val poll = GenomePoll("net", inputIds, outputIds, 3)
    val set = ExercisesSet("randomset", List(
      "random result 0-1",
      "random result 0-1",
      "random result 0-1",
      "random result 0-1",
      "random result 0-1",
      "random result 0-1",
      "random result 0-1",
      "random result 0-1",
      "random result 0-1",
      "random result 0-1"
    ))

    val coach = Coach(set)

    val engine = Engine(coach, 0.5, poll)
    engine.calculateResults()
    val best1 = engine.best
    val result1 = coach.test(best1.data)

    engine.run()

    val best2 = engine.best
    val result2 = coach.test(best2.data)

    assertTrue(result2 >= result1)
    println(s"new result: $result2, old result: $result1")
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

    val poll = GenomePoll(template, inputIds, outputIds, 3)
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
      "random result 0-1",
      "random result 0-1",
      "random result 0-1",
      "random result 0-1",
      "random result 0-1",
      "random result 0-1",
      "random result 0-1",
      "random result 0-1"
    ))

    val coach = Coach(set)

    val engine = Engine(coach, 0.5, poll)
    engine.calculateResults()
    val best1 = engine.best
    val result1 = coach.test(best1.data)

    engine.run()

    val best2 = engine.best
    val result2 = coach.test(best2.data)

    assertTrue(result2 >= result1)
    println(s"new result: $result2, old result: $result1")

    val newPoll = engine.poll
    val n0 = newPoll(0)
    val n1 = newPoll(0)
    val n2 = newPoll(0)

    assertNotEquals(n0, d0)
    assertNotEquals(n0, d1)
    assertNotEquals(n0, d2)
    assertNotEquals(n1, d0)
    assertNotEquals(n1, d1)
    assertNotEquals(n1, d2)
    assertNotEquals(n2, d0)
    assertNotEquals(n2, d1)
    assertNotEquals(n2, d2)

  }*/

  @Test def shouldCreateEvolutionDirectory(): Unit ={
    // for that we need:
    // 1. name of the directory
    val dirName = "test-shouldCreateEvolutionDirectory"
    // 2. inputIds
    val inputIds = this.inputIds
    // 3. outputIds
    val outputIds = this.outputIds
    // 4. net template
    val netTemplate = this.netTemplate
    // 5. exercises set
    val exercisesSet = ExercisesSet("randomset", List("random result 0-1"))

    // create the engine
    Engine(dirName, inputIds, outputIds, netTemplate, exercisesSet)
    // check if the directory exists
    val evolutionDirs = new File(Context().evolutionDir).listFiles.filter(_.isDirectory)
    assertTrue(evolutionDirs.map(_.getName).toSet.contains(dirName))

    // delete directory
    val dir = evolutionDirs.find(_.getName == dirName).get
    try {
      assertTrue(dir.delete())
    } catch {
      case ex: IOException => println(ex.getMessage)
    }
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