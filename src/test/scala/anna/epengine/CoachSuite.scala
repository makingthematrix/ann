package anna.epengine

import anna.Context
import anna.async._
import anna.logger.LOG._
import anna.utils.Utils
import org.junit.Test
import anna.async.NetBuilderOps._
import org.junit.Assert._

/**
 * Created by gorywoda on 05.01.15.
 */
class CoachSuite extends MySuite {

  val f = (wrapper: NetWrapper, success: Double, failure: Double) => {
    var counter = 0
    wrapper.addAfterFire("out1"){ counter += 1 }

    wrapper += "1,1,1,1,1,1"

    wrapper.tickUntilCalm()
    if(counter == 6) success else failure
  }

  @Test def shouldPassNetTest() = {
    // this is the net from DelaySuite.shouldGiveConstantOutput
    builder.inputTickMultiplier = 2.0
    builder.addInput("in1").chain("mi1",1.0).chain("out1",1.0,0.75)
    build()

    // first we check if it works out of the box
    var counter = 0
    netWrapper.addAfterFire("out1"){ counter += 1 }

    netWrapper += "1,1,1,1,1,1"

    netWrapper.tickUntilCalm()
    assertEquals(6, counter)

    shutdown()

    // now let's create a NetData and check if it's valid
    val data = builder.data
    builder.clear()
    builder.set(data)
    build()

    counter = 0
    netWrapper.addAfterFire("out1"){ counter += 1 }

    netWrapper += "1,1,1,1,1,1"

    netWrapper.tickUntilCalm()
    assertEquals(6, counter)

    shutdown()

    // and now let's do the same through the Coach
    val test = Exercise(name = "constant output", inputLen = 1, outputIds = List("out1"), function = f)
    val coach = Coach(List(test))
    val result = coach.test(data)
    assertEquals(1.0, result, 0.01)
  }

  @Test def shouldPassConsecutiveTests() = {
    builder.inputTickMultiplier = 2.0
    builder.addInput("in1").chain("mi1",1.0).chain("out1",1.0,0.75)
    val data = builder.data

    val test1 = Exercise(name = "constant output 1", inputLen = 1, outputIds = List("out1"), function = f)
    val test2 = Exercise(name = "constant output 2", inputLen = 1, outputIds = List("out1"), function = f)
    val test3 = Exercise(name = "constant output 3", inputLen = 1, outputIds = List("out1"), function = f)
    val coach = Coach(List(test1, test2, test3))
    val result = coach.test(data)
    assertEquals(3.0, result, 0.01)
  }

  @Test def shouldPassExercisesSet() = {
    builder.inputTickMultiplier = 2.0
    builder.addInput("in1").chain("mi1",1.0).chain("out1",1.0,0.75)
    val data = builder.data

    val set = ExercisesSet("dummy set", List("constant output for six units") )
    val coach = Coach(set)
    val result = coach.test(data)
    assertEquals(1.0, result, 0.01)
  }

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

  /**
   * This is actually pretty cool: I already know that 'SOSNetWithHushNeuron' generates a valid net which should pass
   * all those exercises. So I can use it to test if the exercises are correct :D
   */
  @Test def shouldPassSOSWithHushNeuron(): Unit = {
    val set = dotSet

    set.validate

    builder.SOSNetWithHushNeuron()
    val data = builder.data
    val result = Coach(set).test(data)

    debug(this, s"the result of ${set.name} is $result")
    assertEquals(set.resultForAllPassed, result, 0.001)
  }

  @Test def shouldSaveAndLoadDotSet(): Unit ={
    val set = dotSet
    set.save
    val loadedSet = ExercisesSet.load(set.name)
    assertEquals(dotSet, loadedSet)
  }
}
