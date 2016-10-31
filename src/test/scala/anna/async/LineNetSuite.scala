package anna.async

import anna.async.NetBuilderOps._
import anna.logger.LOG.debug
import org.junit.Assert._
import org.junit.Test

class LineNetSuite extends MySuite {
  val s = "1,0,0,0,1,0,0,0,1,0,0,0"
  val o = "1,1,0,0,1,1,0,0,1,1,0,0"

  private def lineNet2() = {
    builder.addInput("in1")
           .chain("mi21",0.4,0.6).silence("mi21")
           .chain("out2",1.0)
    build()
    debug("----------")
    val sb = StringBuilder.newBuilder
    netWrapper.addAfterFire("out2"){
      debug("Line!")
      sb.append('-')
    }
    
    sb
  }
  
  @Test def shouldLineThenNothing1(){
    val sb = lineNet2()
    
    netWrapper += "1,1,0,0,0,0"
    init()
    netWrapper.iterateUntilCalm()
    assertEquals("-",sb.toString)
  }
  
  @Test def shouldLine3Times(){
    val sb = lineNet2()
    
    netWrapper += "1,1,0,1,1,0,1,1,0"
    init()
    netWrapper.iterateUntilCalm()
    assertEquals("---",sb.toString)
  }

  private def lineNet3(){

    builder.addInput("in")
           .chain("mi21",0.5,0.55)
           .silence("mi21")
           .chain("line",1.0,0.0).silence("line")
    build()

  }

  @Test def shouldHaveLineInterval3() = {
    lineNet3()
    debug("------------")
    var lines = 0
    netWrapper.addAfterFire("in"){ debug("Incoming!") } 
    netWrapper.addAfterFire("line"){ debug("Line!"); lines += 1; } 

    netWrapper += o
    init()
    val interval = netWrapper.iterateUntilCalm()
    debug(s"interval: $interval, lines: $lines")
    assertEquals(3, lines)

    lines = 0
    netWrapper += s
    netWrapper.iterateUntilCalm()
    debug(s"lines: $lines")
    assertEquals(1, lines)
    // Without the other part of the network recognizing 'dots', two consecutive '1's, even with an interval
    // between them, should be recognized as a line. The interval may mean that the signal is noised.
  }


}