package test.async

import org.scalatest.junit.JUnitSuite
import main.async.NetBuilder

class DotNetSuite extends MySuite {
  private def dotNet() = {
    val builder = NetBuilder()
    builder.defSlope = 5.0
    builder.addInput("in1")
    // dots
    builder.use("in1").chainMiddle("mi11",0.6,0.5).loop("loop1",1.0,0.5,1.0).chainMiddle("mi12",1.0,0.75).chainOutput("out1",1.0)
    builder.use("out1").connect("mi11", -0.49)
    builder.use("out1").connect("mi12", -1.0)
    
    val (in, net, out) = builder.build("in","out")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger("out1", () => {
      println("KROPA!")
      sb.append('.')
    })
    
    (in, sb)
  }
}