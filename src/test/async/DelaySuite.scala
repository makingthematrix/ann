package test.async

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.After
import main._
import main.async.NetInput
import main.async.NetOutput
import scala.concurrent.Promise
import main.async.Neuron
import main.logger.LOG
import main.logger.LOG.debug
import scala.concurrent.Await
import scala.concurrent.duration._
import main.async.NetBuilder
import main.async.Messages._
import main.async.NetRef
import main.async.Context

import scala.collection.mutable

class DelaySuite extends JUnitSuite {
  var builder: NetBuilder = _
  var in: NetInput = _
  var out: NetOutput = _
  var net: NetRef = _
  
  @Before def before(){
    LOG.addLogToStdout()
    builder = NetBuilder()
  }
  
  @After def after(){
    net.shutdown()
    builder = null
    in = null
    out = null
    net = null
    LOG.date()
  }
  
  private def build() = {
    val triple = builder.build("in1","out1")
    in = triple._1
    net = triple._2
    out = triple._3
  }
  
  private def assertEqualsWithTolerance(expected: Seq[Long], received: Seq[Long], tolerance: Long) = {
    assertEquals(expected.size, received.size)
    expected.zip(received).foreach( tuple => if(math.abs(tuple._1 - tuple._2) > tolerance) fail(s"""
        Expected: ${expected}\n
        Received: ${received}\n
        The values ${tuple._1} and ${tuple._2} vary by ${math.abs(tuple._1 - tuple._2)} which is more than the asserted tolerance $tolerance.
    """))
  }
  
  private def produceSeq(size: Int, ini: Long, off: Long):Seq[Long] = size match {
    case 1 => Seq(ini)
    case x if x > 1 => produceSeq(x-1, ini, off) ++ Seq(ini+(x-1)*off)
  }
  
  private def assertOutputAfter(afterMillis: Long, timeoutSeconds: Int) = {
    val p = Promise[Long]
    out.addAfterFireTrigger(out.getId(0), (_:Neuron) => p.success(LOG.time) )

    net.init()
    LOG.timer()
    while(!in.empty) in.tick()
    
    val resultTime = Await.result(p.future, timeoutSeconds seconds).asInstanceOf[Long]
    debug(this,s"resultTime: $resultTime")
    assert(resultTime > afterMillis)
    LOG.date()
  }
  
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
    in.tickInterval = Context.sleepTime * 2;
      
    val list = mutable.ListBuffer[Long]()
    out.find("out1").addAfterFireTrigger("fired", (_:Neuron) => list += LOG.time )
    
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
    in.tickInterval = Context.sleepTime * 2;
      
    val list = mutable.ListBuffer[Long]()
    out.find("out1").addAfterFireTrigger("fired", (_:Neuron) => list += LOG.time )
    
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
    in.tickInterval = Context.sleepTime * 2;
      
    val list = mutable.ListBuffer[Long]()
    out.find("out1").addAfterFireTrigger("fired", (_:Neuron) => list += LOG.time )
    
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
    in.tickInterval = Context.sleepTime * 2;
      
    val list = mutable.ListBuffer[Long]()
    out.find("out1").addAfterFireTrigger("fired", (_:Neuron) => list += LOG.time )
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tickUntilCalm()

    val tolerance = 10L
    assertEqualsWithTolerance(produceSeq(3, tolerance, in.tickInterval * 2), list.toSeq, tolerance)
  }
  
  @Test def shouldCreateOscillator2(){
    builder.addInput("in1").chainMiddle("mi1",1.0).loop("osc",1.0,0.5,-1.0).chainOutput("out1",1.0,0.75)
    builder.use("in1").chainMiddle("mi2",1.0).chainOutput("out2",1.0,0.75)
    builder.use("osc").connect("mi2",-1.0)
    // how about a "hush" signal?
    // how about a synapse with an explicit delay in transferring the signal to another neuron?
    build()
    
    in += "1,1,1,1,1,1"
    in.tickInterval = Context.sleepTime * 4;
    
    val sb = StringBuilder.newBuilder
    out.find("out1").addAfterFireTrigger("fired 1", (_:Neuron) => sb.append('1') )
    out.find("out2").addAfterFireTrigger("fired 0", (_:Neuron) => sb.append('0') )
    
    net.init(usePresleep = false)
    LOG.timer()
    in.tick(6)
    
    val str = sb.toString
    println(str)
    assertEquals("101010",str)
  }
}