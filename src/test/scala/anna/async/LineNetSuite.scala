package anna.async

import anna.async.NetBuilderOps._
import anna.async.logger.LOG.debug
import anna.data.{ForgetValue, HushValue}
import org.junit.Assert._
import org.junit.Test

class LineNetSuite extends MySuite {
  val s = "1,0,0,1,0,0,1,0,0"
  val o = "1,1,0,1,1,0,1,1,0"

  private def lineNet2() = {
    builder.inputTickMultiplicity = 2
    // lines
    builder.addInput("in1").chain("mi21",0.4,0.6,HushValue(),ForgetValue(0.05)).chain("mi22",1.0,0.6).chain("out2",1.0)
    builder.use("mi22").hush("mi21")
    build()
    debug("----------")
    val sb = StringBuilder.newBuilder
    net.addAfterFire("out2"){
      println("KRECHA!")
      sb.append('-')
    }
    
    sb
  }
  
  @Test def shouldLineThenNothing1(){
    val sb = lineNet2()
    
    in += "1,1,0,0,0,0"
    init()
    in.tickUntilCalm()
    assertEquals("-",sb.toString)
  }
  
  @Test def shouldLine3Times(){
    val sb = lineNet2()
    
    in += "1,1,0,1,1,0,1,1,0"
    init()
    in.tickUntilCalm()
    assertEquals("---",sb.toString)
  }
  
  @Test def shouldLine2TimesWithSpace(){
    val sb = lineNet2()
    
    in += "1,1,0,0,1,1,0,0"
    init()
    in.tick(12)
    assertEquals("--",sb.toString)
  }
  
  @Test def shouldNotLine(){
    val sb = lineNet2()
    in += "1,0,0,1,0,0"
    init()
    in.tickUntilCalm()
    assertEquals("",sb.toString)
  }

  private def lineNet3(){
    val itm = 3
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // lines
    builder.use("in").chainMiddle("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / itm)).hush("mi21")
      .chainMiddle("line",1.0,0.0).hush("line")
    build()

  }

  @Test def shouldHaveLineInterval3() = {
    lineNet3()
    debug("------------")
    var lines = 0
    net.addAfterFire("in"){ println("INCOMING!") }
    net.addAfterFire("line"){ println("KRECHA!"); lines += 1; }

    in += o
    init()
    val interval = in.tickUntilCalm()
    println(s"interval: $interval, lines: $lines")
    assertEquals(3, lines)

    lines = 0
    in += s
    in.tickUntilCalm()
    println(s"dots: $lines")
    assertEquals(0, lines)
  }


}