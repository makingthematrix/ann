package test.async

import org.junit.Test
import main.async.Context.sleepTime
import scala.collection.mutable
import main.async.Neuron
import main.async.logger.LOG
import org.junit.Assert._

class DelaySuite extends MySuite {  
  
  @Test def shouldGiveConstantOutput(){
	builder.tickInterval = sleepTime * 2
    builder.addInput("in1").chainMiddle("mi1",1.0).chainOutput("out1",1.0,0.75)
    build()
    
    in += "1,1,1,1,1,1"
      
    val list = mutable.ListBuffer[Long]()
    out.find("out1").addAfterFireTrigger("fired"){ list += LOG.time }
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()
    
    list.foreach(println)
    
    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(6, tolerance, in.tickInterval), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillatorWithMethod1(){
    builder.tickInterval = sleepTime * 2
    builder.addInput("in1").chainMiddle("mi1",1.0).loop("osc",1.0,0.5,-1.0).chainOutput("out1",1.0,0.75)
    build()
    
    in += "1,1,1,1,1,1"
      
    val list = mutable.ListBuffer[Long]()
    out.find("out1").addAfterFireTrigger("fired"){ list += LOG.time }
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()
    
    list.foreach(println)
    
    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(3, tolerance, in.tickInterval * 2), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillatorWithMethod2(){
    builder.tickInterval = sleepTime * 2
    builder.addInput("in").chainMiddle(1.0).oscillator().chainOutput("out1", 1.0, 0.75)
    build()
    
    in += "1,1,1,1,1,1"
      
    val list = mutable.ListBuffer[Long]()
    out.find("out1").addAfterFireTrigger("fired"){ list += LOG.time }
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()

    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(3, tolerance, in.tickInterval * 2), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillator2(){
    builder.tickInterval = sleepTime * 2
    builder.addInput("in1").chainMiddle("mi1",1.0).loop("osc",1.0,0.5,-1.0).chainOutput("out1",1.0,0.75)
    builder.use("osc").chainMiddle("mi2",1.0).chainOutput("out2",1.0,0.75)
    build()
    
    in += "1,1,1,1,1,1"
    
    val sb = StringBuilder.newBuilder
    out.find("out1").addAfterFireTrigger("fired 1"){ sb.append('1') }
    out.find("out2").addAfterFireTrigger("fired 0"){ sb.append('0') }
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()
    
    val str = sb.toString
    println(str)
    assertEquals("101010",str)
  }
}