package test.async

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import main.async.Messages._
import main.logger.LOG.debug
import main.async.Context.sleepTime

class LineNetSuite extends MySuite {
  private def lineNet() = {
    builder.defSlope = 5.0
    // lines
    builder.addInput("in1").chainMiddle("mi21",0.4,0.6).chainMiddle("mi22",1.0,0.6).chainOutput("out2",1.0)
    builder.use("mi22").hush("mi21")
    builder.use("mi21").setForgetting(0.05)
    build()
    debug("----------")
    val sb = StringBuilder.newBuilder
    super.in.tickInterval = sleepTime * 2
    out.addAfterFireTrigger("out2", () => {
      println("KRECHA!")
      sb.append('-')
    })
    
    sb
  }
  
  private def lineNetRes4() = {
    builder.defSlope = 5.0
    builder.resolution = 4
    // lines
    builder.addInput("in1").chainMiddle("mi21",0.13,0.5).chainMiddle("mi22",1.0,0.5).chainOutput("out2",1.0)
    builder.use("mi21").setForgetting(0.02)
    builder.use("mi22").hush("mi21")
    build()
    debug("----------")
    val sb = StringBuilder.newBuilder
    super.in.tickInterval = sleepTime * 2
    out.addAfterFireTrigger("out2", () => {
      println("KRECHA!")
      sb.append('-') 
    })
    
    sb
  }
  
  @Test def shouldLineThenNothing1(){
    val sb = lineNet
    
    in += "1,1,0,0,0,0"
    init()
    in.tickUntilCalm()
    assertEquals("-",sb.toString)
  }
  
  @Test def shouldLine3Times(){
    val sb = lineNet
    
    in += "1,1,0,1,1,0,1,1,0"
    init()
    in.tickUntilCalm()
    assertEquals("---",sb.toString)
  }
  
  @Test def shouldLine2TimesWithSpace(){
    val sb = lineNet
    
    in += "1,1,0,0,1,1,0,0"
    init()
    in.tick(12)
    assertEquals("--",sb.toString)
  }
  
  @Test def shouldNotLine(){
    val sb = lineNet
    in += "1,0,0,1,0,0"
    init()
    in.tickUntilCalm()
    assertEquals("",sb.toString)
  }
  
  @Test def shouldLineThenNothing1Res4(){
    val sb = lineNetRes4
    in += "1,1,0,0,0,0"
    init()
    in.tickUntilCalm()
    assertEquals("-",sb.toString)
  }
  
  @Test def shouldLine3TimesRes4(){
    val sb = lineNetRes4
    in += "1,1,0,1,1,0,1,1,0"
    init()
    in.tickUntilCalm()
    assertEquals("---",sb.toString)
  }
  
  @Test def shouldLine2TimesWithSpaceRes4(){
    val sb = lineNetRes4
    
    in += "1,1,0,0,1,1,0,0"
    init()
    in.tick(12)
    assertEquals("--",sb.toString)
  }
  
  @Test def shouldNotLineRes4(){
    val sb = lineNetRes4
    
    in += "1,0,0,1,0,0"
    init()
    in.tickUntilCalm()
    assertEquals("",sb.toString)
  }
  
  
}