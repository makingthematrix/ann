package test.async.epengine

import java.io.File

import anna.{Commands, Context}
import anna.async.{NetWrapper, NetBuilder}
import anna.async.NetBuilderOps._
import anna.data.{Hush, SynapseWeight}
import anna.epengine._
import anna.logger.LOG
import anna.utils.Utils
import org.junit.Assert._
import org.junit.{After, Before, Test}
import org.scalatest.junit.JUnitSuite


class EngineSuite extends JUnitSuite {

  private var _oldContext:Context = _

  @Before def before() {
    _oldContext = Context()
    LOG.addLogToStdout()
  }

  @After def after(): Unit ={
    if(engine != null && Utils.fileExists(engine.dirPath)) Utils.deleteDir(engine.dirPath)
    Context.set(_oldContext)
  }

  private var engine: Engine = null

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
/*
  @Test def shouldTestGenomePoll(): Unit ={
    val poll = GenomePoll("net", inputIds, outputIds, 3)
    assertEquals(3, poll.genomes.size)

    val results = Coach(List(t1)).test(poll)
    results.foreach( tuple => println(s"${tuple._1.id}: ${tuple._2}"))
  }

  @Test def shouldPerformEvolutionIteration(): Unit ={
    val poll = GenomePoll("net", inputIds, outputIds, 3)
    val coach = Coach(List(t1, t2))

    engine = Engine(coach, poll)
    val best1 = engine.best
    val result1 = coach.test(best1.data)

    engine.run()

    val best2 = engine.best
    val result2 = coach.test(best2.data)

    println(s"new result: $result2, old result: $result1")
    assertTrue(result2 >= result1)
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

    engine = Engine(coach, poll)
    engine.calculateResults()
    val best1 = engine.best
    val result1 = coach.test(best1.data)

    engine.run()

    val best2 = engine.best
    val result2 = coach.test(best2.data)

    println(s"new result: $result2, old result: $result1")
    assertTrue(result2 >= result1)
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

    engine = Engine(coach, poll)
    engine.calculateResults()
    val best1 = engine.best
    val result1 = coach.test(best1.data)

    engine.run()

    val best2 = engine.best
    val result2 = coach.test(best2.data)

    println(s"new result: $result2, old result: $result1")
    assertTrue(result2 >= result1)

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

  }

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
    val exercisesSet = this.exercisesSet

    // create the engine
    engine = Engine(dirName, inputIds, outputIds, netTemplate, exercisesSet)
    // check if the directory exists
    val evolutionDirs = new File(Context().evolutionDir).listFiles.filter(_.isDirectory)
    assertTrue(evolutionDirs.map(_.getName).toSet.contains(dirName))
  }

  @Test def shouldSaveContextSetTemplateAndPoll(): Unit ={
    val dirName = "test-shouldSaveContextSetAndPoll"
    engine = Engine(dirName, inputIds, outputIds, netTemplate, exercisesSet)
    val dir = new File(Context().evolutionDir).listFiles.find(f => f.getName == dirName && f.isDirectory).get
    val filesInDir = dir.listFiles.map( f => (f.getAbsolutePath.substring(f.getAbsolutePath.lastIndexOf('/')+1) , f)).toMap
    filesInDir.foreach( tuple => println(tuple._1))
    assertTrue(filesInDir.contains("context.json"))
    assertTrue(filesInDir.contains("netTemplate.json"))
    assertTrue(filesInDir.contains("exercisesSet.json"))
    assertTrue(filesInDir.contains("poll_iteration0.json"))
  }

  @Test def shouldSaveAndRestoreContextFromJson(): Unit ={
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

  @Test def shouldReadEngineFromDir(): Unit ={
    val dirName = "test-shouldReadEngineFromDir"
    engine = Engine(dirName, inputIds, outputIds, netTemplate, exercisesSet)

    val genomeId = engine.poll.ids(0)
    println(s"genomeId: $genomeId")

    val engine2 = Engine(dirName)

    assertEquals(engine.coach.exercises, engine2.coach.exercises)

    val g1 = engine.poll(genomeId)
    val g2 = engine2.poll(genomeId)

    println("-------")
    println(g1.toJson)
    println("-------")
    println(g2.toJson)
    println("-------")

    assertEquals(g1.toJson, g2.toJson)
  }

  @Test def shouldThrowExceptionIfDirDoesNotExist(): Unit ={
    val dirName = "test-shouldThrowExceptionIfDirDoesNotExist"
    try {
      engine = Engine(dirName)
      fail()
    } catch {
      case ex: IllegalArgumentException =>
        if(!ex.getMessage.contains(dirName)) fail()
      case other: Throwable => fail(s"Unknown exception: $other")
    }
  }

  @Test def shouldSaveProgress(): Unit ={
    val dirName = "test-shouldSaveProgress"
    engine = Engine(dirName, inputIds, outputIds, netTemplate, exercisesSet)
    assertTrue(Utils.fileExists(engine.dirPath + "/poll_iteration0.json"))
    assertFalse(Utils.fileExists(engine.dirPath + "/results_iteration0.json"))
    assertFalse(Utils.fileExists(engine.dirPath + "/poll_iteration1.json"))
    assertFalse(Utils.fileExists(engine.dirPath + "/results_iteration1.json"))

    engine.run()

    assertTrue(Utils.fileExists(engine.dirPath + "/poll_iteration0.json"))
    assertTrue(Utils.fileExists(engine.dirPath + "/results_iteration0.json"))
    assertTrue(Utils.fileExists(engine.dirPath + "/poll_iteration1.json"))
    assertTrue(Utils.fileExists(engine.dirPath + "/results_iteration1.json"))
  }

  @Test def shouldSaveLogs(): Unit ={
    val dirName = "test-shouldSaveLogs"
    engine = Engine(dirName, inputIds, outputIds, netTemplate, exercisesSet)
    assertFalse(Utils.fileExists(engine.dirPath + "/iteration1.log"))
    assertFalse(Utils.fileExists(engine.dirPath + "/mutations_iteration1.log"))

    engine.run()

    assertTrue(Utils.fileExists(engine.dirPath + "/iteration1.log"))
    assertTrue(Utils.fileExists(engine.dirPath + "/mutations_iteration1.log"))
  }

  @Test def shouldNeitherCrossNorMutate(): Unit ={
    val dirName = "test-shouldNeitherCrossNorMutate"
    engine = Engine(dirName, inputIds, outputIds, netTemplate, exercisesSet)

    Context.withMutationProbability(0.0)
    Context.withCrossCoefficient(0.0)

    engine.run()

    assertTrue(Utils.fileExists(engine.dirPath + "/mutations_iteration1.log"))
    val lines = Utils.load(engine.dirPath + "/mutations_iteration1.log").split("\n")
    assertTrue(lines.filterNot(_.contains("CLONING:")).isEmpty)
  }*/

  @Test def shouldCloneTheBestGenome(): Unit = {
    val ex = new Exercise("shouldCloneTheBestGenome-exercise", 1, List("out1")) {
      def run(wrapper: NetWrapper): Double = wrapper.net.id match {
        case "best" => 1.0
        case _ => 0.5
      }
    }

    Context.withMutationProbability(1.0)
    Context.withCrossCoefficient(1.0)

    val map = NetGenome.accessMap(List("in1"), List("out1"))

    val g1 = NetGenome(NetBuilder("best").addInput("in1").chain("out1",0.5,0.5).data, map)
    val g2 = NetGenome(NetBuilder("other-1").addInput("in1").chain("out1",0.0,0.0).data, map)
    val g3 = NetGenome(NetBuilder("other-2").addInput("in1").chain("out1",1.0,1.0).data, map)

    val engine = Engine(Coach(List(ex)), GenomePoll(List(g1, g2, g3)))
    val str1 = g1.data.toJson
    println("---\n"+str1+"---\n")

    engine.calculateResults()

    val diff1 = Commands.diff(g1.data, engine.best.data)
    println(diff1)
    //assertEquals("", diff1)
    val str2 = engine.best.data.toJson
    println("---\n"+str2+"---\n")
    assertEquals(str1, str2)

    engine.run(1)

    //val diff2 = Commands.diff(g1.data, engine.best.data)
   // assertEquals("", diff1)
    val str3 = engine.best.data.toJson
    println("---\n"+str3+"---\n")
    assertEquals(str1, str3)
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