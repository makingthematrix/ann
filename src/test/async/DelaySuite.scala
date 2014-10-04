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
    LOG.date()
  }
  
  private def build() = {
    val triple = builder.build("in1","out1")
    in = triple._1
    net = triple._2
    out = triple._3
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
      
    assertOutputAfter(50L, 5)
  }
  
  @Test def shouldSendOutputWithMoreDelay_usingInputSynapse(){
    builder.addInput().chainMiddle(0.501,0.5).loop(1.0,0.5,1.0).chainOutput(1.0,0.75)
    build()
    
    in += "1"
      
    assertOutputAfter(50L, 5)
  }
  
  @Test def shouldSendOutputWithMoreDelay_usingSlopeAndSelf(){
    builder.addInput().chainMiddle(0.55,0.5,8.0).self(1.0).chainOutput(1.0,0.75)
    build()
    
    in += "1"
      
    assertOutputAfter(50L, 5)
  }
  
  @Test def shouldSendOutputWith2Signals_usingTreshold(){
    builder.addInput().chainMiddle(0.4,0.75,5.0).loop(1.0,0.5,1.0).chainOutput(1.0,0.9)
    build()
    
    in += "1,1"
      
    assertOutputAfter(100L, 5)
  }
  
  
}