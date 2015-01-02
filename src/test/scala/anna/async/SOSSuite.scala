package anna.async

import anna.async.NetBuilderOps._
import anna.logger.LOG
import anna.logger.LOG.debug
import anna.data.{ForgetValue, HushValue}
import org.junit.Assert._
import org.junit.Test

import scala.util.Random

class SOSSuite extends MySuite {
  val s = "1,0,0,1,0,0,1,0,0"
  val o = "1,1,0,1,1,0,1,1,0"

  private def dotLineNet(){
    val itm = 3.0
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // dots
    builder.use("in").chain("mi11",1.0,0.0,HushValue((2 * itm).toInt)).hush("mi11")
                     .chain("mi12",1.0,0.0).loop("loop",1.0,0.0,1.0)
                     .chain("dot",0.6/(2.0*itm),0.6).hush("mi12").hush("loop").hush("dot")
    // lines
    builder.use("in").chain("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / itm))
                     .hush("mi21")
                     .chain("line",1.0,0.0).hush("line")
                     
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
    val itm = 3.0
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // dots
    builder.use("in").chain("mi11",1.0,0.0,HushValue((2 * itm).toInt)).hush("mi11")
                     .chain("mi12",1.0,0.0).loop("loop",1.0,0.0,1.0)
                     .chain("dot",0.6/(2.0*itm),0.6).hush("mi12").hush("loop").hush("dot")
                     .chain("S",0.5,0.81)
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
    val itm = 3.0
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // lines
    builder.use("in").chain("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / itm)).hush("mi21")
                     .chain("line",1.0,0.0).hush("line")    
                     .chain("O",0.6,0.81)
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
    val itm = 3.0
    builder.inputTickMultiplicity = itm
    builder.addInput("in")
    // dots
    builder.use("in").chain("mi11",1.0,0.0,HushValue((2 * itm).toInt)).hush("mi11")
                     .chain("mi12",1.0,0.0).loop("loop",1.0,0.0,1.0)
                     .chain("dot",0.6/(2.0*itm),0.6).hush("mi12").hush("loop").hush("dot") 
                     .chain("S",0.5,0.81)
    // lines
    builder.use("in").chain("mi21",0.5,0.55,HushValue(),ForgetValue(0.4 / itm)).hush("mi21")
                     .chain("line",1.0,0.0).hush("line")
                     .chain("O",0.6,0.81)
                     
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
  
  private def SOSNetWithHushNeuron(){
    val itm = 3.0
    builder.inputTickMultiplicity = itm
    builder.defSlope = 5.0
    builder.addInput("in")
    // dots
    builder.use("in").chain("mi11",1.0,0.0,HushValue((2 * itm).toInt)).hush("mi11")
                     .chain("mi12",1.0,0.0).loop("loop",1.0,0.0,1.0)
                     .chain("dot",0.6/(2.0*itm),0.6)
                     .chain("S",0.5,0.81)
    builder.addHushNeuron("dot_hush").hush("mi12").hush("loop").hush("dot")
    builder.use("dot").hush("dot_hush")
    
    // lines
    builder.use("in").chain("mi21",0.55,0.58,HushValue(),ForgetValue(0.4 / itm)).hush("mi21")
                     .chain("line",1.0,0.0).hush("line")
                     .chain("O",0.6,0.81)
                     
    // if line then not dot
    builder.use("line").hush("dot_hush")

    // if S then not O, if O then not S...
    builder.use("S").chainHushNeuron("hush_letters").hush("S").hush("O")
    builder.use("O").hush("hush_letters")
    
    build()

    net.addAfterFire("in"){ println("INCOMING!") }
    net.addAfterFire("dot"){ println("KROPA!") }
    net.addAfterFire("line"){ println("KRECHA!") }
    
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

  def shouldTestNoise(shouldBe: String, noisedSignal: String, noised1: Double = 0.9, noised0: Double = 0.1) = {
    SOSNetWithHushNeuron()

    val sb = StringBuilder.newBuilder
    net.addAfterFire("S"){ sb.append('S') }
    net.addAfterFire("O"){ sb.append('O') }

    in.regSign('i',noised1)
    in.regSign('_',noised0)

    LOG.timer()

    in += noisedSignal
    in.tickUntilCalm()
    assertEquals(shouldBe, sb.toString)

    LOG.date()
  }

  @Test def shouldUnnoiseS() = shouldTestNoise("S",s)
  @Test def shouldUnnoiseSForwardShift1() = shouldTestNoise("S","0,1,0,0,1,0,0,1,0")
  @Test def shouldUnnoiseSForwardShift2() = shouldTestNoise("S","0,0,1,0,0,1,0,0,1")
  @Test def shouldUnnoiseSNoiseAtTheEnd() = shouldTestNoise("S","1,0,0,1,0,0,1,0,0,1")
  @Test def shouldUnnoiseO() = shouldTestNoise("O",o)
  @Test def shouldUnnoiseOForwardShift1() = shouldTestNoise("O","0,1,1,0,1,1,0,1,1")
  @Test def shouldUnnoiseONoiseAtTheEnd() = shouldTestNoise("O","1,1,0,1,1,0,1,1,0,1")
  @Test def shouldUnnoiseONoiseInTheMiddle() = shouldTestNoise("O","1,1,0,1,1,1,1,1,0")
  @Test def shouldUnnoiseSUnclearSignals() = shouldTestNoise("S","i,_,_,i,_,_,i,_,_")
  @Test def shouldUnnoiseOUnclearSignals() = shouldTestNoise("O","i,i,_,i,i,_,i,i,_")

  @Test def shouldUnnoiseSOS() = {
    val noisedS = "i,_,_,i,_,_,i,_,_"
    val noisedO = "i,i,_,i,i,_,i,i,_"
    shouldTestNoise("SOS",noisedS + "," + noisedO + "," + noisedS)
  }

  @Test def shouldUnnoiseVaried() = {
    def vary(d: Double) = d + (Random.nextDouble() * 0.1 - 0.05)
    def V = vary(0.95)
    def v = vary(0.05)

    val inputSignal = List(V,v,v,V,v,v,V,v,v,V,V,v,V,V,v,V,V,v,V,v,v,V,v,v,V,v,v)

    SOSNetWithHushNeuron()

    val sb = StringBuilder.newBuilder
    net.addAfterFire("S"){ sb.append('S') }
    net.addAfterFire("O"){ sb.append('O') }

    LOG.timer()

    inputSignal.foreach( in += _ )
    in.tickUntilCalm()
    assertEquals("SOS", sb.toString)

    LOG.date()
  }
}