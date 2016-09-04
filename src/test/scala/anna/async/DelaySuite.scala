package anna.async

import anna.Context
import anna.async.NetBuilderOps._
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test

import scala.collection.mutable

class DelaySuite extends MySuite {  
  
  @Test def shouldGiveConstantOutput(){
    builder.addInput("in1").chain("mi1",1.0).chain("out1",1.0,0.75)
    build()
    
    netWrapper += "1,1,1,1,1,1"
      
    val list = mutable.ListBuffer[Long]()
    netWrapper.addAfterFire("out1","fired")( (_:Double)=>{ list += LOG.time } )
    
    LOG.timer()
    netWrapper.tickUntilCalm()
    
    list.foreach(println)
    
    val tolerance = Context().tickTime
    assertEqualsWithTolerance(produceSeq(6, tolerance, Context().tickTime), list.toSeq, tolerance)
  }

  @Test def shouldCreateOscillatorWithMethod2() {
    builder.addInput("in").chain("mi1", 1.0).oscillator().chain("out1", 1.0, 0.75)
    build()

    netWrapper += "1,1,1,1,1,1"

    val list = mutable.ListBuffer[Int]()
    netWrapper.addAfterFire("out1", "fired")((_: Double) => {
      list += netWrapper.iteration
    })

    netWrapper.tickUntilCalm()

    assertEquals(List(0, 2, 4), list.toList)
  }
  
  @Test def shouldCreateOscillator2(){
    builder.addInput("in1").chain("mi1",1.0).loop("osc",1.0,0.5,-1.0).chain("out1",1.0,0.75)
    builder.use("osc").chain("mi2",1.0).chain("out2",1.0,0.75)
    build()
    
    netWrapper += "1,1,1,1,1,1"
    
    val sb = StringBuilder.newBuilder
    netWrapper.addAfterFire("out1","fired 1")( (_:Double)=>{ sb.append('1') } )
    netWrapper.addAfterFire("out1","fired 0")( (_:Double)=>{ sb.append('0') } )
    
    LOG.timer()
    netWrapper.tickUntilCalm()
    
    val str = sb.toString
    println(str)
    assertEquals("101010",str)
  }
}