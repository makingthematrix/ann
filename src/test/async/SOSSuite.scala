package test.async

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import main.async.Messages._
import main.async.logger.LOG.debug
import main.async.logger.LOG
import main.async.Context
import main.data.HushValue
import main.data.ForgetValue
import main.data.ForgetAll

class SOSSuite extends MySuite {
  val s = "1,0,0,1,0,0,1,0,0"
  val o = "1,1,0,1,1,0,1,1,0"
  
  private def dotNet(){
    val itm = 3
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // dots
    builder.use("in").chainMiddle("mi11",1.0,0.0,HushValue(2 * itm)).hush("mi11")
                     .chainMiddle("mi12",1.0,0.0).loop("loop1",1.0,0.0,1.0)
                     .chainMiddle("dot",0.6/(2.0*itm),0.6).hush("mi12").hush("loop1").hush("dot")                          
    build()
    
  }
  
  @Test def shouldHaveDotInterval3() = {
    dotNet()
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
  
  private def lineNet(){
    val itm = 3
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // lines
    builder.use("in").chainMiddle("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / itm)).hush("mi21")
                     .chainMiddle("line",1.0,0.0).hush("line")                          
    build()
    
  }
  
  @Test def shouldHaveLineInterval3() = {
    lineNet()
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
  
  private def dotLineNet(){
    val itm = 3
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // dots
    builder.use("in").chainMiddle("mi11",1.0,0.0,HushValue(2 * itm)).hush("mi11")
                     .chainMiddle("mi12",1.0,0.0).loop("loop",1.0,0.0,1.0)
                     .chainMiddle("dot",0.6/(2.0*itm),0.6).hush("mi12").hush("loop").hush("dot") 
    // lines
    builder.use("in").chainMiddle("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / itm)).hush("mi21")
                     .chainMiddle("line",1.0,0.0).hush("line")
                     
    // if line then not dot
    builder.use("line").hush("mi12").hush("loop").hush("dot")
    
    build()
    
    net.addAfterFire("in"){ println("INCOMING!") }
    
    debug("------------")
  }
  
  @Test def shouldHaveDotsAndLines() = {
    dotLineNet()
    var dots = 0; net.addAfterFire("dot"){ println("KROPA!"); dots += 1; }
    var lines = 0; net.addAfterFire("line"){ println("KRECHA!"); lines += 1; }
    init()
    
    in += s
    in.tickUntilCalm()
    println(s"dots: $dots, lines: $lines")
    assertEquals(3, dots)
    assertEquals(0, lines)
    
    dots = 0; lines = 0;
    in += o
    in.tickUntilCalm()
    println(s"dots: $dots, lines: $lines")
    assertEquals(0, dots)
    assertEquals(3, lines)
  }
  
  @Test def shouldGuessDotsAndLines() = {
    dotLineNet()
    val sb = StringBuilder.newBuilder
    net.addAfterFire("dot"){ sb.append('.') }
    net.addAfterFire("line"){ sb.append('-') }
    init()
    
    in += "1,0,0"
    in.tickUntilCalm()
    assertEquals(".",sb.toString)
    
    sb.clear()
    
    in += "0,1,0"
    in.tickUntilCalm()
    assertEquals(".",sb.toString)
    
    sb.clear()
    
    in += "0,0,1"
    in.tickUntilCalm()
    assertEquals(".",sb.toString)
    
    sb.clear()
    
    in += "1,0,1"
    in.tickUntilCalm()
    assertEquals(".",sb.toString)
    
    sb.clear()
    
    in += "1,1,0"
    in.tickUntilCalm()
    assertEquals("-",sb.toString)
    
    sb.clear()
    
    in += "0,1,1"
    in.tickUntilCalm()
    assertEquals("-",sb.toString)
    
    sb.clear()
    
    in += "1,1,1"
    in.tickUntilCalm()
    assertEquals("-",sb.toString)  
  }
  
  private def SNet(){
    val itm = 3
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // dots
    builder.use("in").chainMiddle("mi11",1.0,0.0,HushValue(2 * itm)).hush("mi11")
                     .chainMiddle("mi12",1.0,0.0).loop("loop",1.0,0.0,1.0)
                     .chainMiddle("dot",0.6/(2.0*itm),0.6).hush("mi12").hush("loop").hush("dot")
                     .chainMiddle("S",0.5,0.81)
    build()
    
    net.addAfterFire("in"){ println("INCOMING!") }

    debug("------------")
  }
  
  @Test def shouldHaveSInterval3() = {
    SNet()
    
    var dots = 0; net.addAfterFire("dot"){ println("KROPA!"); dots += 1; } 
    var S = 0; net.addAfterFire("S"){ println("S!"); S += 1; }
    
    in += s
    init()
    val interval = in.tickUntilCalm()
    println(s"interval: $interval, dots: $dots, S: $S")
    assertEquals(3, dots)
    assertEquals(1, S)
    
    dots = 0
    S = 0
    in += o
    in.tickUntilCalm()
    println(s"dots: $dots, S: $S")
    assertEquals(3, dots)
    assertEquals(1, S)
  }
  
  private def ONet(){
    val itm = 3
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // lines
    builder.use("in").chainMiddle("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / itm)).hush("mi21")
                     .chainMiddle("line",1.0,0.0).hush("line")    
                     .chainMiddle("O",0.6,0.81)
    build()
    
    net.addAfterFire("in"){ println("INCOMING!") }

    debug("------------")
  }
  
  @Test def shouldHaveOInterval3() = {
    ONet()
    
    var lines = 0; net.addAfterFire("line"){ println("KRECHA!"); lines += 1; }
    var O = 0; net.addAfterFire("O"){ println("O!"); O += 1; }
    
    in += o
    init()
    val interval = in.tickUntilCalm()
    println(s"interval: $interval, lines: $lines, O: $O")
    assertEquals(3, lines)
    assertEquals(1, O)
    
    lines = 0
    O = 0
    in += s
    in.tickUntilCalm()
    println(s"lines: $lines, O: $O")
    assertEquals(0, lines)
    assertEquals(0, O)
  }
  
  private def SOSNet(){
    val itm = 3
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // dots
    builder.use("in").chainMiddle("mi11",1.0,0.0,HushValue(iterations = 2 * itm)).hush("mi11")
                     .chainMiddle("mi12",1.0,0.0).loop("loop",1.0,0.0,1.0)
                     .chainMiddle("dot",0.6/(2.0*itm),0.6).hush("mi12").hush("loop").hush("dot") 
                     .chainMiddle("S",0.5,0.81)
    // lines
    builder.use("in").chainMiddle("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / itm)).hush("mi21")
                     .chainMiddle("line",1.0,0.0).hush("line")
                     .chainMiddle("O",0.6,0.81)
                     
    // if line then not dot
    builder.use("line").hush("mi12").hush("loop").hush("dot")
    
    build()

    net.addAfterFire("in"){ println("INCOMING!") }
    
    debug("------------")
  }
  
  @Test def shouldHaveSOSInterval3() = {
    SOSNet()
    
    var dots = 0; net.addAfterFire("dot"){ println("KROPA!"); dots += 1; } 
    var S = 0; net.addAfterFire("S"){ println("S!"); S += 1; }
    var lines = 0; net.addAfterFire("line"){ println("KRECHA!"); lines += 1; }
    var O = 0; net.addAfterFire("O"){ println("O!"); O += 1; }
    
    init()

    in += s
    in.tickUntilCalm()
    println(s"dots: $dots, S: $S, lines: $lines, O: $O")
    assertEquals(3, dots)
    assertEquals(1, S)
    assertEquals(0, lines)
    assertEquals(0, O)
    
    dots = 0; S = 0; lines = 0; O = 0;
    
    in += o
    in.tickUntilCalm()
    println(s"dots: $dots, S: $S, lines: $lines, O: $O")
    assertEquals(0, dots)
    assertEquals(0, S)
    assertEquals(3, lines)
    assertEquals(1, O)
    
    dots = 0; S = 0; lines = 0; O = 0;
    
    in += s
    in.tickUntilCalm()
    println(s"dots: $dots, S: $S, lines: $lines, O: $O")
    assertEquals(3, dots)
    assertEquals(1, S)
    assertEquals(0, lines)
    assertEquals(0, O)
  }
  
  @Test def shouldHaveSOSTogether() = {
    SOSNet()
    
    val sb = StringBuilder.newBuilder
    net.addAfterFire("S"){ sb.append('S') }
    net.addAfterFire("O"){ sb.append('O') }
    
    in += s
    in += o
    in += s
    
    init()
    in.tickUntilCalm()
    assertEquals("SOS", sb.toString)
  }

  @Test def shouldTestNoises() = {
    SOSNet()
    
    val sb = StringBuilder.newBuilder
    net.addAfterFire("S"){ sb.append('S') }
    net.addAfterFire("O"){ sb.append('O') }
    
    init()

    // S
    in += "1,0,0,1,0,0,1,0,0"
    in.tickUntilCalm()
    assertEquals("S", sb.toString)
    
    sb.clear()
    
    // forward shift by one
    in += "0,1,0,0,1,0,0,1,0"
    in.tickUntilCalm()
    assertEquals("S", sb.toString)
    
    sb.clear()
    
    // forward shift by two
    in += "0,0,1,0,0,1,0,0,1"
    in.tickUntilCalm()
    assertEquals("S", sb.toString)
    
    sb.clear()
    
    // noise at the end
    in += "1,0,0,1,0,0,1,0,0,1"
    in.tickUntilCalm()
    assertEquals("S", sb.toString)
     
    sb.clear()
    
    // O
    in += "1,1,0,1,1,0,1,1,0"
    in.tickUntilCalm()
    assertEquals("O", sb.toString)
    
    sb.clear()
    
    // forward shift by one
    in += "0,1,1,0,1,1,0,1,1"
    in.tickUntilCalm()
    assertEquals("O", sb.toString)
    
    sb.clear()
    
    // noise at the end
    in += "1,1,0,1,1,0,1,1,0,1"
    in.tickUntilCalm()
    assertEquals("O", sb.toString)
    
    sb.clear()
    
    // noise in the middle
    in += "1,1,0,1,1,1,1,1,0"
    in.tickUntilCalm()
    assertEquals("O", sb.toString)
  }
  
  private def SOSNetWithHushNeuron(){
    val itm = 3
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // dots
    builder.use("in").chainMiddle("mi11",1.0,0.0,HushValue(iterations = 2 * itm)).hush("mi11")
                     .chainMiddle("mi12",1.0,0.0).loop("loop",1.0,0.0,1.0)
                     .chainMiddle("dot",0.6/(2.0*itm),0.6)
                     .chainMiddle("S",0.5,0.81)
    builder.addHushNeuron("dot_hush").hush("mi12").hush("loop").hush("dot")
    builder.use("dot").hush("dot_hush")
    
    // lines
    builder.use("in").chainMiddle("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / itm)).hush("mi21")
                     .chainMiddle("line",1.0,0.0).hush("line")
                     .chainMiddle("O",0.6,0.81)
                     
    // if line then not dot
    builder.use("line").hush("dot_hush")
    
    build()

    net.addAfterFire("in"){ println("INCOMING!") }
    
    debug("------------")
  }
  
  @Test def shouldHaveSOSWithHushNeuron() = {
    SOSNetWithHushNeuron()
    
    val sb = StringBuilder.newBuilder
    net.addAfterFire("S"){ sb.append('S') }
    net.addAfterFire("O"){ sb.append('O') }
    
    in += s
    in += o
    in += s
    
    init()
    in.tickUntilCalm()
    assertEquals("SOS", sb.toString)
  }
}