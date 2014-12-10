package test.async

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import main.async.Messages._
import main.async.logger.LOG.debug
import main.async.HushValue
import main.async.ForgetValue

class LineNetSuite extends MySuite {
  private def lineNet() = {
    builder.inputTickMultiplicity = 2
    // lines
    builder.addInput("in1").chain("mi21",0.4,0.6,HushValue(),ForgetValue(0.05)).chain("mi22",1.0,0.6).chain("out2",1.0)
    builder.use("mi22").hush("mi21")
    build()
    debug("----------")
    val sb = StringBuilder.newBuilder
    net.addAfterFireTrigger("out2"){
      println("KRECHA!")
      sb.append('-')
    }
    
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
  
}