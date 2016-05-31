package anna.async

import anna.async.NetBuilderOps._
import anna.data.ForgetValue
import anna.logger.LOG.debug
import org.junit.Assert._
import org.junit.Test

class LineNetSuite extends MySuite {
  val s = "1,0,0,1,0,0,1,0,0"
  val o = "1,1,0,1,1,0,1,1,0"

  private def lineNet2() = {
    builder.addInput("in1")
           .chain("mi21",0.4,0.6,ForgetValue(0.1)).hush("mi21")
           .chain("out2",1.0)
    build()
    debug("----------")
    val sb = StringBuilder.newBuilder
    netWrapper.addAfterFire("out2")( (_:Double)=>{
      println("KRECHA!")
      sb.append('-')
    })
    
    sb
  }
  
  @Test def shouldLineThenNothing1(){
    val sb = lineNet2()
    
    netWrapper += "1,1,0,0,0,0"
    init()
    netWrapper.tickUntilCalm()
    assertEquals("-",sb.toString)
  }
  
  @Test def shouldLine3Times(){
    val sb = lineNet2()
    
    netWrapper += "1,1,0,1,1,0,1,1,0"
    init()
    netWrapper.tickUntilCalm()
    assertEquals("---",sb.toString)
  }

  private def lineNet3(){

    builder.addInput("in")
           .chain("mi21",0.5,0.55,ForgetValue(0.2))
           .hush("mi21")
           .chain("line",1.0,0.0).hush("line")
    build()

  }

  @Test def shouldHaveLineInterval3() = {
    lineNet3()
    debug("------------")
    var lines = 0
    netWrapper.addAfterFire("in")( (_:Double)=>{ println("INCOMING!") } )
    netWrapper.addAfterFire("line")( (_:Double)=>{ println("KRECHA!"); lines += 1; } )

    netWrapper += o
    init()
    val interval = netWrapper.tickUntilCalm()
    println(s"interval: $interval, lines: $lines")
    assertEquals(3, lines)

    lines = 0
    netWrapper += s
    netWrapper.tickUntilCalm()
    println(s"dots: $lines")
    assertEquals(0, lines)
  }


}