package anna.async

import anna.async.NetBuilderOps._
import anna.logger.LOG.debug
import anna.data.HushValue
import org.junit.Assert._
import org.junit.Test

class DotNetSuite extends MySuite {
  val s = "1,0,0,1,0,0,1,0,0"
  val o = "1,1,0,1,1,0,1,1,0"

  private def dotNet2() = {
    builder.inputTickMultiplicity = 2
    builder.addInput("in1")
    // dots
    builder.use("in1").chain("mi11",0.6,0.5).loop("loop1",1.0,0.5,1.0).chain("mi12",1.0,0.75).chain("out1",1.0)
    builder.use("out1").connect("mi11", -0.49)
    builder.use("out1").connect("mi12", -1.0)

    build()
    debug("----------")
    val sb = StringBuilder.newBuilder
    net.addAfterFire("out1"){
      println("KROPA!")
      sb.append('.')
     }

    sb
  }

  @Test def shouldDotThenNothing(){
    val sb = dotNet2()
    
    in += "1"
    init()
    in.tickUntilCalm()
    assertEquals(".",sb.toString)
  }
  
  @Test def shouldDot3Times(){
    val sb = dotNet2()
    
    in += s

    init()
    in.tickUntilCalm()
    assertEquals("...",sb.toString)
  }

  private def dotNet3(){
    val itm = 3
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // dots
    builder.use("in").chain("mi11",1.0,0.0,HushValue(2 * itm)).hush("mi11")
      .chain("mi12",1.0,0.0).loop("loop1",1.0,0.0,1.0)
      .chain("dot",0.6/(2.0*itm),0.6).hush("mi12").hush("loop1").hush("dot")
    build()
  }

  @Test def shouldHaveDotInterval3() = {
    dotNet3()
    debug("------------")
    var dots = 0
    net.addAfterFire("in"){ println("INCOMING!") }
    net.addAfterFire("dot"){ println("KROPA!"); dots += 1; }

    in += s
    init()
    val interval = in.tickUntilCalm()
    println(s"interval: $interval, dots: $dots")
    assertEquals(3, dots)

    dots = 0
    in += o
    in.tickUntilCalm()
    println(s"dots: $dots")
    assertEquals(3, dots)
  }
}