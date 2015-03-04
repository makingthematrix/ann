package anna.async

import anna.Context
import Context.tickTime
import anna.async.NetBuilderOps._
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test

import scala.collection.mutable

class DelaySuite extends MySuite {  
  
  @Test def shouldGiveConstantOutput(){
	  builder.inputTickMultiplier = 2.0
    builder.addInput("in1").chain("mi1",1.0).chain("out1",1.0,0.75)
    build()
    
    netWrapper += "1,1,1,1,1,1"
      
    val list = mutable.ListBuffer[Long]()
    netWrapper.addAfterFire("out1","fired"){ list += LOG.time }
    
    LOG.timer()
    netWrapper.tickUntilCalm()
    
    list.foreach(println)
    
    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(6, tolerance, (netWrapper.inputTickMultiplier * tickTime).toLong), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillatorWithMethod1(){
    builder.inputTickMultiplier = 2.0
    builder.addInput("in1").chain("mi1",1.0).loop("osc",1.0,0.5,-1.0).chain("out1",1.0,0.75)
    build()
    LOG.debug(this,"here!")
    
    netWrapper += "1,1,1,1,1,1"
      
    val list = mutable.ListBuffer[Long]()
    netWrapper.addAfterFire("out1","fired"){ list += LOG.time }
    
    LOG.timer()
    netWrapper.tickUntilCalm()
    
    list.foreach(println)
    
    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(3, tolerance, (netWrapper.inputTickMultiplier * tickTime * 2).toLong), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillatorWithMethod2(){
    builder.inputTickMultiplier = 2.0
    builder.addInput("in").chain("mi1",1.0).oscillator().chain("out1", 1.0, 0.75)
    build()
    
    netWrapper += "1,1,1,1,1,1"
      
    val list = mutable.ListBuffer[Long]()
    netWrapper.addAfterFire("out1","fired"){ list += LOG.time }
    
    LOG.timer()
    netWrapper.tickUntilCalm()

    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(3, tolerance, (netWrapper.inputTickMultiplier * tickTime * 2).toLong), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillator2(){
    builder.inputTickMultiplier = 2.0
    builder.addInput("in1").chain("mi1",1.0).loop("osc",1.0,0.5,-1.0).chain("out1",1.0,0.75)
    builder.use("osc").chain("mi2",1.0).chain("out2",1.0,0.75)
    build()
    
    netWrapper += "1,1,1,1,1,1"
    
    val sb = StringBuilder.newBuilder
    netWrapper.addAfterFire("out1","fired 1"){ sb.append('1') }
    netWrapper.addAfterFire("out1","fired 0"){ sb.append('0') }
    
    LOG.timer()
    netWrapper.tickUntilCalm()
    
    val str = sb.toString
    println(str)
    assertEquals("101010",str)
  }
}