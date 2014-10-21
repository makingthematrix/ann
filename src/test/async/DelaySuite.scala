package test.async

import org.junit.Test
import main.async.Messages.ForgetValue
import main.async.Messages.ForgetAll
import main.async.Context.sleepTime
import scala.collection.mutable
import main.async.Neuron
import main.logger.LOG
import org.junit.Assert._

class DelaySuite extends MySuite {  
  @Test def shouldSendOutputWithDelay_usingInputSynapse(){
    builder.addInput().chainMiddle(0.55,0.5).loop(1.0,0.5,1.0).chainOutput(1.0,0.75)
    build()
    
    in += "1"
      
    assertOutputAfter(50L, 5)
  }
  
  @Test def shouldSendOutputWithDelay_usingSlopeAndSelf(){
    builder.addInput().chainMiddle(0.7,0.5,5.0).self(1.0).chainOutput(1.0,0.75)
    build()
    
    in += "1"
      
    assertOutputAfter(200L, 5)
  }
  
  @Test def shouldSendOutputWithMoreDelay_usingInputSynapseAndForgetting(){ 
    builder.addInput().chainMiddle(0.501,0.5).loop(1.0,0.5,1.0).chainOutput(1.0,0.75,ForgetValue(1.0))
    build()
    
    in += "1"
      
    assertOutputAfter(280L, 5)
  }
  
  @Test def shouldSendOutputWithMoreDelay_usingSlopeAndLoopAndForgetting(){
    builder.addInput().chainMiddle(0.55,0.5,5.0).loop(1.0,0.5,0.75).chainOutput(1.0,0.75,ForgetValue(1.0))
    build()
    
    in += "1"
      
    assertOutputAfter(280L, 5)
  }
  
  @Test def shouldSendOutputWithMoreDelay_usingSlopeAndLoopAndForgettingAll(){
    builder.addInput().chainMiddle(0.51,0.5,2.5).loop(1.0,0.5,1.0).chainOutput(1.0,0.75,ForgetAll)
    build()
    
    in += "1"
      
    assertOutputAfter(350L, 5)
  }
  
  @Test def shouldSendOutputWith2Signals_usingTreshold(){
    builder.addInput().chainMiddle(0.4,0.75,5.0).loop(1.0,0.5,1.0).chainOutput(1.0,0.9)
    build()
    in.tickInterval = 100L
    
    in += "1,1"
      
    assertOutputAfter(200L, 5)
  }
  
  @Test def shouldGiveConstantOutput(){
    builder.addInput("in1").chainMiddle("mi1",1.0).chainOutput("out1",1.0,0.75)
    build()
    
    in += "1,1,1,1,1,1"
    in.tickInterval = sleepTime * 2;
      
    val list = mutable.ListBuffer[Long]()
    out.find("out1").addAfterFireTrigger("fired", () => list += LOG.time )
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()
    
    list.foreach(println)
    
    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(6, tolerance, in.tickInterval), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillator(){
    builder.addInput("in1").chainMiddle("mi1",1.0).loop("osc",1.0,0.5,-1.0).chainOutput("out1",1.0,0.75)
    build()
    
    in += "1,1,1,1,1,1"
    in.tickInterval = sleepTime * 2;
      
    val list = mutable.ListBuffer[Long]()
    out.find("out1").addAfterFireTrigger("fired", () => list += LOG.time )
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()
    
    list.foreach(println)
    
    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(3, tolerance, in.tickInterval * 2), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillatorWithMethod1(){
    builder.addInput().chainMiddle(1.0).oscillator().chainOutput("out1", 1.0, 0.75)
    build()
    
    in += "1,1,1,1,1,1"
    in.tickInterval = sleepTime * 2;
      
    val list = mutable.ListBuffer[Long]()
    out.find("out1").addAfterFireTrigger("fired", () => list += LOG.time )
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()

    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(3, tolerance, in.tickInterval * 2), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillatorWithMethod2(){
    builder.addInput().chainOscillator(1.0).chainOutput("out1", 1.0, 0.75)
    build()
    
    in += "1,1,1,1,1,1"
    in.tickInterval = sleepTime * 2;
      
    val list = mutable.ListBuffer[Long]()
    out.find("out1").addAfterFireTrigger("fired", () => list += LOG.time )
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()

    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(3, tolerance, in.tickInterval * 2), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillator2(){
    builder.addInput("in1").chainMiddle("mi1",1.0).loop("osc",1.0,0.5,-1.0).chainOutput("out1",1.0,0.75)
    builder.use("osc").chainMiddle("mi2",1.0).chainOutput("out2",1.0,0.75)
    build()
    
    in += "1,1,1,1,1,1"
    in.tickInterval = sleepTime * 2;
    
    val sb = StringBuilder.newBuilder
    out.find("out1").addAfterFireTrigger("fired 1", () => sb.append('1') )
    out.find("out2").addAfterFireTrigger("fired 0", () => sb.append('0') )
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()
    
    val str = sb.toString
    println(str)
    assertEquals("101010",str)
  }
}