package anna.async

import anna.Context
import anna.async.NetBuilderOps._
import anna.async.TestUtils._
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

import scala.collection.mutable

class DelaySuite extends JUnitSuite {

  @Test def shouldGiveConstantOutput(){
    val netWrapper = NetBuilder().addInput("in1").chain("mi1",1.0).chain("out1",1.0,0.75).build("net")
    
    netWrapper += "1,1,1,1,1,1"

    LOG.timer()
    val list = mutable.ListBuffer[Long]()
    netWrapper.addAfterFire("out1","fired"){ list += LOG.time }
    
    netWrapper.iterateUntilCalm()

    val tolerance = Context().iterationTime
    assertEqualsWithTolerance(produceSeq(6, tolerance, Context().iterationTime), list, tolerance)

    netWrapper.shutdown()
  }

  @Test def shouldCreateOscillatorWithMethod2() {
    val netWrapper = NetBuilder().addInput("in").chain("mi1", 1.0).oscillator().chain("out1", 1.0, 0.75).build()

    netWrapper += "1,1,1,1,1,1"

    val list = mutable.ListBuffer[Int]()
    netWrapper.addAfterFire("out1", "fired"){
      list += netWrapper.iteration
    }

    netWrapper.iterateUntilCalm()

    assertEquals(List(1, 3, 5), list.toList)

    netWrapper.shutdown()
  }

  @Test def shouldCreateOscillator2(){
    val netWrapper = NetBuilder().addInput("in1").chain("mi1",1.0).loop("osc",1.0,0.5,-1.0).chain("out1",1.0,0.75)
                                 .use("osc").chain("mi2",1.0).chain("out2",1.0,0.75)
                                 .build()

    netWrapper += "1,1,1,1,1,1"

    val sb = StringBuilder.newBuilder
    netWrapper.addAfterFire("out1","fired 1"){ sb.append('1') }
    netWrapper.addAfterFire("out1","fired 0"){ sb.append('0') }

    netWrapper.iterateUntilCalm()

    assertEquals("101010", sb.toString)

    netWrapper.shutdown()
  }
}