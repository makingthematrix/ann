package anna.epengine

import anna.async._
import org.junit.Test
import anna.async.NetBuilderOps._
import org.junit.Assert._

/**
 * Created by gorywoda on 05.01.15.
 */
class TesterSuite extends MySuite {

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

    // now let's create a NetData and check if it's valid*/
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

    // and now let's do the same through the Tester
    val test = NetTest(name = "constant output", inputLen = 1, outputIds = List("out1"), function = f)
    val tester = Tester(List(test))
    val result = tester.test(data)
    assertEquals(1.0, result, 0.01)
  }

  @Test def shouldPassConsecutiveTests() = {
    builder.inputTickMultiplier = 2.0
    builder.addInput("in1").chain("mi1",1.0).chain("out1",1.0,0.75)
    val data = builder.data

    val test1 = NetTest(name = "constant output 1", inputLen = 1, outputIds = List("out1"), function = f)
    val test2 = NetTest(name = "constant output 2", inputLen = 1, outputIds = List("out1"), function = f)
    val test3 = NetTest(name = "constant output 3", inputLen = 1, outputIds = List("out1"), function = f)
    val tester = Tester(List(test1, test2, test3))
    val result = tester.test(data)
    assertEquals(3.0, result, 0.01)
  }
}
