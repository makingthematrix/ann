package anna.epengine

import anna.async._
import org.junit.Test
import anna.async.NetBuilderOps._
import org.junit.Assert._

/**
 * Created by gorywoda on 05.01.15.
 */
class TesterSuite extends MySuite {

  @Test def shouldPassNetTest() = {
    // this is the net from DelaySuite.shouldGiveConstantOutput
    builder.inputTickMultiplier = 2.0
    builder.addInput("in1").chain("mi1",1.0).chain("out1",1.0,0.75)
    build()

    // first we check if it works out of the box
    var counter = 0
    net.addAfterFire("out1"){ counter += 1 }

    in += "1,1,1,1,1,1"

    in.tickUntilCalm()
    assertEquals(6, counter)

    shutdown()

    // now let's create a NetData and check if it's valid*/
    val data = builder.data
    builder.clear()
    builder.set(data)
    build()

    counter = 0
    net.addAfterFire("out1"){ counter += 1 }

    in += "1,1,1,1,1,1"

    in.tickUntilCalm()
    assertEquals(6, counter)

    shutdown()

    // and now let's do the same through the Tester
    val f = (input: NetInput, netRef: NetRef, success: Double, failure: Double) => {
      var counter = 0
      netRef.addAfterFire("out1"){ counter += 1 }

      input += "1,1,1,1,1,1"

      input.tickUntilCalm()
      if(counter == 6) success else failure
    }

    val test = NetTest(name = "constant output", inputLen = 1, outputIds = List("out1"), function = f)
    val tester = Tester(List(test))
    val result = tester.test(data)
    assertEquals(1.0, result, 0.01)
  }
}
