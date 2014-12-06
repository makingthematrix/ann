package test.async

import org.junit.Test
import main.async.Context.tickTime
import scala.collection.mutable
import main.async.Neuron
import main.async.logger.LOG
import org.junit.Assert._

class DelaySuite extends MySuite {  
  
  @Test def shouldGiveConstantOutput(){
	builder.inputTickMultiplicity = 2
    builder.addInput("in1").chain("mi1",1.0).chain("out1",1.0,0.75)
    build()
    
    in += "1,1,1,1,1,1"
      
    val list = mutable.ListBuffer[Long]()
    net.addAfterFireTrigger("out1","fired"){ list += LOG.time }
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()
    
    list.foreach(println)
    
    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(6, tolerance, in.inputTickMultiplicity * tickTime), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillatorWithMethod1(){
    builder.inputTickMultiplicity = 2
    builder.addInput("in1").chain("mi1",1.0).loop("osc",1.0,0.5,-1.0).chain("out1",1.0,0.75)
    build()
    
    in += "1,1,1,1,1,1"
      
    val list = mutable.ListBuffer[Long]()
    net.addAfterFireTrigger("out1","fired"){ list += LOG.time }
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()
    
    list.foreach(println)
    
    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(3, tolerance, in.inputTickMultiplicity * tickTime * 2), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillatorWithMethod2(){
    builder.inputTickMultiplicity = 2
    builder.addInput("in").chain("mi1",1.0).oscillator().chain("out1", 1.0, 0.75)
    build()
    
    in += "1,1,1,1,1,1"
      
    val list = mutable.ListBuffer[Long]()
    net.addAfterFireTrigger("out1","fired"){ list += LOG.time }
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()

    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(3, tolerance, in.inputTickMultiplicity * tickTime * 2), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillator2(){
    builder.inputTickMultiplicity = 2
    builder.addInput("in1").chain("mi1",1.0).loop("osc",1.0,0.5,-1.0).chain("out1",1.0,0.75)
    builder.use("osc").chain("mi2",1.0).chain("out2",1.0,0.75)
    build()
    
    in += "1,1,1,1,1,1"
    
    val sb = StringBuilder.newBuilder
    net.addAfterFireTrigger("out1","fired 1"){ sb.append('1') }
    net.addAfterFireTrigger("out1","fired 0"){ sb.append('0') }
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()
    
    val str = sb.toString
    println(str)
    assertEquals("101010",str)
  }
}