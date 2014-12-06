package test.async

import org.scalatest.junit.JUnitSuite
import main.async.NetBuilder
import org.junit.Test
import org.junit.Assert._
import main.async.logger.LOG.debug
import main.async.logger.LOG

class DotNetSuite extends MySuite {
  private def dotNet() = {
    builder.inputTickMultiplicity = 2
    builder.addInput("in1")
    // dots
    builder.use("in1").chainMiddle("mi11",0.6,0.5).loop("loop1",1.0,0.5,1.0).chain("mi12",1.0,0.75).chain("out1",1.0)
    builder.use("out1").connect("mi11", -0.49)
    builder.use("out1").connect("mi12", -1.0)
    
    build()
    debug("----------")
    val sb = StringBuilder.newBuilder
    net.addAfterFireTrigger("out1"){
      println("KROPA!")
      sb.append('.')
     }
    
    sb
  }
  
  @Test def shouldDotThenNothing(){
    val sb = dotNet
    
    in += "1"
    init()
    in.tickUntilCalm()
    assertEquals(".",sb.toString)
  }
  
  @Test def shouldDot3Times(){
    val sb = dotNet
    
    in += "1,0,0,1,0,0,1,0,0" // is there a way to just send 1s? after all in this implementations 0s are just lack of signal
    init()
    in.tickUntilCalm()
    assertEquals("...",sb.toString)
  }

  
}