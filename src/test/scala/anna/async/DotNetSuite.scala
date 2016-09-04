package anna.async

import anna.async.NetBuilderOps._
import anna.data.HushValue
import anna.logger.LOG.debug
import org.junit.Assert._
import org.junit.Test

class DotNetSuite extends MySuite {
  val s = "1,0,0,0,1,0,0,0,1,0,0,0"
  val o = "1,1,0,0,1,1,0,0,1,1,0,0"

  private def dotNet3(){
    builder.addInput("in")
    // dots
    builder.use("in").chain("mi11",1.0,0.0,HushValue(2)).hush("mi11")
      .chain("mi12",1.0,0.0).loop("loop1",1.0,0.0,1.0)
      .chain("dot",0.6/2.0,0.6).hush("mi12").hush("loop1").hush("dot")
    build()
  }

  @Test def shouldHaveDotInterval3() = {
    dotNet3()
    debug("------------")

    var dots = 0
    netWrapper += s

    init()
    netWrapper.addAfterFire("in")( (_:Double)=>{ println("INCOMING!") } )
    netWrapper.addAfterFire("dot")( (_:Double)=>{ println("KROPA!"); dots += 1; } )

    val interval = netWrapper.tickUntilCalm()
    println(s"interval: $interval, dots: $dots")
    assertEquals(3, dots)

    dots = 0
    netWrapper += o

    // without the other part of the network which would recognize the "line" signals,
    // lines should also be recognized as dots - maybe the additional '1's mean only that the signal is noised

    val interval2 = netWrapper.tickUntilCalm()
    println(s"interval: $interval, dots: $dots")
    assertEquals(3, dots)
  }
}