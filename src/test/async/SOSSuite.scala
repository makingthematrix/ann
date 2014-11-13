package test.async

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import main.async.Messages._
import main.logger.LOG.debug
import main.logger.LOG
import main.async.Context.sleepTime
import main.async.Context
import main.async.HushValue
import main.async.ForgetValue
import main.async.ForgetAll

class SOSSuite extends MySuite {
  val s = "1,0,0,1,0,0,1,0,0"
  val o = "1,1,0,1,1,0,1,1,0"
     /* 
  private def foo(){
    builder.defSlope = 5.0
    builder.resolution = 4
          
    builder.addInput("in1").chainMiddle("mi11",0.23,0.5).loop("loop1",0.75,0.5,1.0).chainMiddle("mi12",1.0,0.75).chainMiddle("dot",1.0)
    //builder.addInput("in1").dummy("mi11",0.55).loop("loop1",1.0,0.5,0.99).chainMiddle("mi12",1.0,0.66).chainMiddle("dot",1.0)
    
    builder.use("mi12").hush("mi11").hush("loop1")
    //builder.use("dot").hush("mi11").hush("mi12").hush("loop1")
    builder.use("dot").chainMiddle("S",0.5,0.9)//.setForgetting(0.01)
         
    builder.use("in1").chainMiddle("mi21",0.13,0.5,HushValue(),ForgetValue(0.02)).chainMiddle("mi22",1.0,0.5).chainMiddle("line",1.0)
    //builder.use("mi22").hush("mi21")
    //builder.use("mi22").connect("mi12",-1.0).connect("loop1",-1.0)
    builder.use("line").chainMiddle("O",0.9,0.9)
    builder.use("O").hush("S").hush("dot")
  }
  
  private def firstTry(){
    builder.defSlope = 5.0
    builder.resolution = 4
    builder.addInput("in1").chainMiddle("mi11",0.22,0.5).loop("loop1",0.75,0.5,1.0).chainMiddle("mi12",1.0,0.75).chainMiddle("dot",1.0)
    builder.use("mi12").hush("mi11").hush("loop1")
    builder.use("dot").chainMiddle("S",0.5,0.9)
         
    builder.use("in1").chainMiddle("mi21",0.13,0.5,HushValue(),ForgetValue(0.02)).chainMiddle("mi22",1.0,0.5).chainMiddle("line",1.0).chainMiddle("O",0.9)
    builder.use("O").hush("S").hush("dot")    
  }  
  
  private def secondTry(){
    builder.defSlope = 5.0
    builder.addInput("in1")
    // dots
    builder.use("in1").chainMiddle("mi11",0.6,0.5,HushValue(),ForgetAll).loop("loop1",1.0,0.5,1.0).chainMiddle("mi12",1.0,0.75).chainMiddle("dot",1.0).chainMiddle("S",0.5,0.9)
    builder.use("dot")/*.hush("mi11")*/.hush("loop1").hush("mi12")//.connect("mi11", -0.49).connect("mi12", -1.0)
         
    // lines
    builder.use("in1").chainMiddle("mi21",0.4,0.6,HushValue()).chainMiddle("mi22",1.0,0.6).chainMiddle("line",1.0).chainMiddle("O",0.9,0.9)
    //builder.use("mi21").setForgetting(0.05)
    
    // if line then not dot
    builder.use("mi22").hush("mi21").hush("mi11").hush("loop1")
    builder.use("line").hush("dot").connect("mi12",-1.0)
    builder.use("O").hush("S").hush("dot")     
  }
  
  private def thirdTry(){
    builder.defSlope = 5.0
    builder.addInput("in1")
    // dots
    builder.use("in1").chainMiddle("mi11",0.6,0.5,HushValue(),ForgetAll)
                      .loop("loop1",1.0,0.5,1.0)
                      .chainMiddle("mi12",1.0,0.75,HushValue())
                      .chainMiddle("dot",1.0)
                      .chainMiddle("S",0.5,0.9)
    builder.use("dot").hush("loop1").hush("mi12")
         
    // lines
    builder.use("in1").chainMiddle("mi21",0.4,0.6,HushValue(),ForgetValue(0.07))
                      .chainMiddle("mi22",1.0,0.6)
                      .chainMiddle("line",1.0)
                      .chainMiddle("O",0.9,0.9)
    
    // if line then not dot
    builder.use("mi22").hush("mi21").hush("mi12").hush("mi11").hush("loop1")
    builder.use("line").hush("dot").hush("mi12")
    //builder.use("dot").hush("mi21").hush("mi22")
    builder.use("O").hush("S").hush("dot")     
  }
    
  private lazy val sosNet = {
    builder.tickInterval = Context.sleepTime * 2
    
    thirdTry()

    build()
    debug("------------")
    net.addAfterFireTrigger("dot"){ println("KROPA!") }
    net.addAfterFireTrigger("line"){ println("KRECHA!") }
    val sb = StringBuilder.newBuilder
    net.addAfterFireTrigger("S"){
      println("S!")
      sb.append('S')
    }
 
    net.addAfterFireTrigger("O"){
      println("O!")
      sb.append('O'); 
    }

    sb
  }
    
  @Test def shouldReturnS() = {
    val sb = sosNet
    //LOG.allow("dot","S")
   // LOG.trackAll = false
    in += s
    init()
    val interval = in.tickUntilCalm()
    assertEquals("S",sb.toString)
    println(s"interval: $interval")
    LOG.clearAllowedIds()
  }
  
  @Test def shouldNotReturnLine() = {
    val sb = sosNet
    var lineFired = false
    net.addAfterFireTrigger("line", "lineNot"){ lineFired = true }
    net.find("dot").neuronOpt.get
    LOG.allow("dot","S","line","O")
    LOG.trackAll = false
    in += s
    init()
    val interval = in.tickUntilCalm()
    assertEquals(false, lineFired)
    println(s"interval: $interval")
    LOG.clearAllowedIds()
  }
  
  @Test def shouldReturnO() = {
    val sb = sosNet
   // LOG.allow("line","O")
    //LOG.trackAll = false
    in += o
    init()
    val interval = in.tickUntilCalm()
    assertEquals("O",sb.toString)
    println(s"interval: $interval")
    LOG.clearAllowedIds()
  }
  
  @Test def shouldReturnSOS() = {
    val sb = sosNet
    LOG.allow("line","O","dot","S")
    LOG.trackAll = false
    in += s
    in += o
    in += s
    init()
    val interval = in.tickUntilCalm()
    assertEquals("SOS",sb.toString)
    println(s"interval: $interval")
    LOG.clearAllowedIds()
  }
  
  @Test def shouldHaveDotRes1() = {
    builder.defSlope = 5.0
    builder.addInput("in")
    // dots
    builder.use("in").chainMiddle("mi11",1.0,0.7,HushValue(),ForgetValue(0.5)).hush("mi11")
                     .chainMiddle("mi12",1.0,0.0).loop("loop1",1.0,0.5,1.0)
                     .chainMiddle("dot",0.4,0.6).hush("mi12").hush("loop1").hush("dot")

    build()
    debug("------------")
    var dots = 0
    net.addAfterFireTrigger("in"){ println("INCOMING!") }
    net.addAfterFireTrigger("dot"){ println("KROPA!"); dots += 1; }
    
    in += s
    init()
    val interval = in.tickUntilCalm()
    println(s"interval: $interval, dots: $dots")
    assertEquals(3, dots)
  }*/
  
  private def dotNet(){
    val res = 3
    builder.tickInterval = sleepTime * res
    builder.addInput("in")
    // dots
    builder.use("in").chainMiddle("mi11",1.0,0.7,HushValue(iterations = 2 * res)).hush("mi11")
                     .chainMiddle("mi12",1.0,0.0).loop("loop1",1.0,0.5,1.0)
                     .chainMiddle("dot",0.1,0.6).hush("mi12").hush("loop1").hush("dot")                          
    build()
    
  }
  
  @Test def shouldHaveDotInterval3() = {
    dotNet()
    debug("------------")
    var dots = 0
    net.addAfterFireTrigger("in"){ println("INCOMING!") }
    net.addAfterFireTrigger("dot"){ println("KROPA!"); dots += 1; }
    
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
    val res = 3
    builder.tickInterval = sleepTime * res
    builder.addInput("in")
    // lines
    builder.use("in").chainMiddle("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / res)).hush("mi21")
                     .chainMiddle("line",1.0,0.0).hush("line")                          
    build()
    
  }
  
  @Test def shouldHaveLineInterval3() = {
    lineNet()
    debug("------------")
    var lines = 0
    net.addAfterFireTrigger("in"){ println("INCOMING!") }
    net.addAfterFireTrigger("line"){ println("KRECHA!"); lines += 1; }
    
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
    val res = 3
    builder.tickInterval = sleepTime * res
    builder.addInput("in")
    // dots
    builder.use("in").chainMiddle("mi11",1.0,0.7,HushValue(iterations = 2 * res)).hush("mi11")
                     .chainMiddle("mi12",1.0,0.0).loop("loop",1.0,0.5,1.0)
                     .chainMiddle("dot",0.1,0.6).hush("mi12").hush("loop").hush("dot") 
    // lines
    builder.use("in").chainMiddle("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / res)).hush("mi21")
                     .chainMiddle("line",1.0,0.0).hush("line")
                     
    // if line then not dot
    builder.use("line").hush("mi12").hush("loop").hush("dot")
    
    build()
    
    net.addAfterFireTrigger("in"){ println("INCOMING!") }
    
    debug("------------")
  }
  
  @Test def shouldHaveDotsAndLines() = {
    dotLineNet()
    var dots = 0; net.addAfterFireTrigger("dot"){ println("KROPA!"); dots += 1; }
    var lines = 0; net.addAfterFireTrigger("line"){ println("KRECHA!"); lines += 1; }
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
  
  private def SNet(){
    val res = 3
    builder.tickInterval = sleepTime * res
    builder.addInput("in")
    // dots
    builder.use("in").chainMiddle("mi11",1.0,0.7,HushValue(iterations = 2 * res)).hush("mi11")
                     .chainMiddle("mi12",1.0,0.0).loop("loop",1.0,0.5,1.0)
                     .chainMiddle("dot",0.1,0.6).hush("mi12").hush("loop").hush("dot")
                     .chainMiddle("S",0.5,0.81)
    build()
    
    net.addAfterFireTrigger("in"){ println("INCOMING!") }

    debug("------------")
  }
  
  @Test def shouldHaveSInterval3() = {
    SNet()
    
    var dots = 0; net.addAfterFireTrigger("dot"){ println("KROPA!"); dots += 1; } 
    var S = 0; net.addAfterFireTrigger("S"){ println("S!"); S += 1; }
    
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
    val res = 3
    builder.tickInterval = sleepTime * res
    builder.addInput("in")
    // lines
    builder.use("in").chainMiddle("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / res)).hush("mi21")
                     .chainMiddle("line",1.0,0.5).hush("line")    
                     .chainMiddle("O",0.6,0.81)
    build()
    
    net.addAfterFireTrigger("in"){ println("INCOMING!") }

    debug("------------")
  }
  
  @Test def shouldHaveOInterval3() = {
    ONet()
    
    var lines = 0; net.addAfterFireTrigger("line"){ println("KRECHA!"); lines += 1; }
    var O = 0; net.addAfterFireTrigger("O"){ println("O!"); O += 1; }
    
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
    val res = 3
    builder.tickInterval = sleepTime * res
    builder.addInput("in")
    // dots
    builder.use("in").chainMiddle("mi11",1.0,0.7,HushValue(iterations = 2 * res)).hush("mi11")
                     .chainMiddle("mi12",1.0,0.0).loop("loop",1.0,0.5,1.0)
                     .chainMiddle("dot",0.1,0.6).hush("mi12").hush("loop").hush("dot") 
                     .chainMiddle("S",0.5,0.81)
    // lines
    builder.use("in").chainMiddle("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / res)).hush("mi21")
                     .chainMiddle("line",1.0,0.5).hush("line")
                     .chainMiddle("O",0.6,0.81)
                     
    // if line then not dot
    builder.use("line").hush("mi12").hush("loop").hush("dot")
    
    build()

    net.addAfterFireTrigger("in"){ println("INCOMING!") }
    
    debug("------------")
  }
  
  @Test def shouldHaveSOSInterval3() = {
    SOSNet()
    
    var dots = 0; net.addAfterFireTrigger("dot"){ println("KROPA!"); dots += 1; } 
    var S = 0; net.addAfterFireTrigger("S"){ println("S!"); S += 1; }
    var lines = 0; net.addAfterFireTrigger("line"){ println("KRECHA!"); lines += 1; }
    var O = 0; net.addAfterFireTrigger("O"){ println("O!"); O += 1; }
    
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
    net.addAfterFireTrigger("S"){ sb.append('S') }
    net.addAfterFireTrigger("O"){ sb.append('O') }
    
    in += s
    in += o
    in += s
    
    init()
    in.tickUntilCalm()
    assertEquals("SOS", sb.toString)
  }
  // 000 -> ''
  // 100 -> .
  // 010 -> .
  // 001 -> .
  // 110 -> -
  // 011 -> -
  // 101 -> .
  // 111 -> -
  
  // v1. przenieść tickInterval do buildera, by można było go ustawić tam, a nie w samym in
  // 2. przenieść mechanizm tickUntilCalm do net. na koniec net powinna wysyłać do neuronów Hush, 
  //    a do in wiadomość, że już jest "calm".
  // v3. wywalić Hush.value; wystarczy iterations, a value zawsze = 0.0
  // v4. zmienić forgetting w Neuron z var na val
  // v5. builder.defSlope domyślnie = 5.0, żeby nie trzeba było tego za każdym razem ustawiać
  // v6. zmienić addAfterFireTrigger tak, aby można było pisać net.addAfterFireTrigger("S"){ sb.append('S') }
  // v7. wywalić Messages.Forget i Context.forgettingGranularity
  // 8. wywalić niedziałające unit testy, a działające przeorganizować ;)
  // 9. zrobić osobny logger dla synca i asynca
  // 10. zapisać na githubie jako "stable"
  // 11. wycinanie niepotrzebnego kodu: wywalić 'sync', NetOutput, output neurony z NetBuildera, NetRef i Net, delay z synaps.
  // 12. zapisać na githubie jako "SOS v1.0"
}