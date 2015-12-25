package anna.epengine

import anna.async.{NetBuilder, NetWrapper}
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import org.junit.Assert._

import anna.async.NetBuilderOps._

import anna.logger.LOG._

/**
 * Created by gorywoda on 12/24/15.
 */
class ExercisesSuite extends JUnitSuite {
  lazy val sosNetData = NetBuilder().SOSNetData()

  @Test def shouldFinishAfterTimeout(){
    val exerciseTime = 200L
    val lessThanExerciseTime = 100L
    val moreThanExerciseTime = 300L
    val expectedResult = 1.0
    val timeoutResult = 0.0

    val timeoutOK = new Exercise("timeout exercise ok", 0, Nil, moreThanExerciseTime, timeoutResult) {
      def run(wrapper: NetWrapper):Double = {
        Thread.sleep(exerciseTime)
        expectedResult
      }
    }

    assertEquals(expectedResult, timeoutOK.runWithTimeout(null), 0.01)

    val timeoutFailed = new Exercise("timeout exercise failed", 0, Nil, lessThanExerciseTime, timeoutResult) {
      def run(wrapper: NetWrapper):Double = {
        Thread.sleep(exerciseTime)
        expectedResult
      }
    }

    assertEquals(timeoutResult, timeoutFailed.runWithTimeout(null), 0.01)
  }

  @Test def testcountSOEqually1SOSNetTime(): Unit ={
    val timeCap = 5000L
    val sosNet = NetBuilder(sosNetData).build()
    val offset = System.currentTimeMillis()
    val result = ExercisesLibrary.countSOEqually1.run(sosNet)
    val runTime = System.currentTimeMillis() - offset
    println(s"result: $result, runTime: $runTime")
    assertTrue(timeCap > runTime)
    sosNet.shutdown()
  }

  @Test def testcountSOEqually2SOSNetTime(): Unit ={
    val timeCap = 10000L
    val sosNet = NetBuilder(sosNetData).build()
    val offset = System.currentTimeMillis()
    val result = ExercisesLibrary.countSOEqually2.run(sosNet)
    val runTime = System.currentTimeMillis() - offset
    println(s"result: $result, runTime: $runTime")
    assertTrue(timeCap > runTime)
    sosNet.shutdown()
  }

  @Test def testcountSOEqually3SOSNetTime(): Unit ={
    val timeCap = 15000L
    val sosNet = NetBuilder(sosNetData).build()
    val offset = System.currentTimeMillis()
    val result = ExercisesLibrary.countSOEqually3.run(sosNet)
    val runTime = System.currentTimeMillis() - offset
    println(s"result: $result, runTime: $runTime")
    assertTrue(timeCap > runTime)
    sosNet.shutdown()
  }
}
