package test.async

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import main.async.Messages._
import main.logger.LOG.debug
import main.logger.LOG
import main.async.Context.sleepTime
import main.async.Context

class SOSSuite extends MySuite {
  val s = "1,0,0,1,0,0,1,0,0"
  val o = "1,1,0,1,1,0,1,1,0"
      
  private def foo(){
    builder.defSlope = 5.0
    builder.resolution = 4
          
    builder.addInput("in1").chainMiddle("mi11",0.23,0.5).loop("loop1",0.75,0.5,1.0).chainMiddle("mi12",1.0,0.75).chainMiddle("dot",1.0)
    //builder.addInput("in1").dummy("mi11",0.55).loop("loop1",1.0,0.5,0.99).chainMiddle("mi12",1.0,0.66).chainMiddle("dot",1.0)
    
    builder.use("mi12").hush("mi11").hush("loop1")
    //builder.use("dot").hush("mi11").hush("mi12").hush("loop1")
    builder.use("dot").chainMiddle("S",0.5,0.9)//.setForgetting(0.01)
         
    builder.use("in1").chainMiddle("mi21",0.13,0.5).chainMiddle("mi22",1.0,0.5).chainMiddle("line",1.0)
    builder.use("mi21").setForgetting(0.02)
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
         
    builder.use("in1").chainMiddle("mi21",0.13,0.5).chainMiddle("mi22",1.0,0.5).chainMiddle("line",1.0)
    builder.use("mi21").setForgetting(0.02)
    builder.use("O").hush("S").hush("dot")    
  }  
  
  private def secondTry(){
    builder.defSlope = 5.0
    builder.addInput("in1")
    // dots
    builder.use("in1").chainMiddle("mi11",0.6,0.5).loop("loop1",1.0,0.5,1.0).chainMiddle("mi12",1.0,0.75).chainMiddle("dot",1.0).chainMiddle("S",0.5,0.9)
    builder.use("dot").connect("mi11", -0.49).connect("mi12", -1.0)
         
    // lines
    builder.use("in1").chainMiddle("mi21",0.4,0.6).chainMiddle("mi22",1.0,0.6).chainMiddle("line",1.0).chainMiddle("O",0.9,0.9)
    builder.use("mi21").setForgetting(0.05)
    
    // if line then not dot
    builder.use("mi22").hush("mi21").connect("mi12",-1.0).connect("loop1",-1.0)
    builder.use("line").hush("dot").connect("mi12",-1.0)
    builder.use("O").hush("S").hush("dot")       
  }
    
  private lazy val sosNet = {
    builder.defSlope = 5.0
    builder.defHushValue = 0.0
    //builder.resolution = 4
    builder.addInput("in1")
    // dots
    builder.use("in1").chainMiddle("mi11",0.6,0.5).loop("loop1",1.0,0.5,1.0).chainMiddle("mi12",1.0,0.75).chainMiddle("dot",1.0).chainMiddle("S",0.5,0.9)
    builder.use("dot").connect("mi11", -0.49).connect("mi12", -1.0)      
    //builder.addInput("in1").chainMiddle("mi11",0.23,0.5).loop("loop1",0.75,0.5,1.0).chainMiddle("mi12",1.0,0.75).chainMiddle("dot",1.0)
    //builder.addInput("in1").dummy("mi11",0.55).loop("loop1",1.0,0.5,0.99).chainMiddle("mi12",1.0,0.66).chainMiddle("dot",1.0)
    
    //builder.use("mi12").hush("mi11").hush("loop1")
    //builder.use("dot").hush("mi11").hush("mi12").hush("loop1")
    //builder.use("dot").chainMiddle("S",0.5,0.9)//.setForgetting(0.01)
         
        // lines
    builder.use("in1").chainMiddle("mi21",0.4,0.6).chainMiddle("mi22",1.0,0.6).chainMiddle("line",1.0).chainMiddle("O",0.9,0.9)
    builder.use("mi21").setForgetting(0.05)
    
    //builder.use("in1").chainMiddle("mi21",0.13,0.5).chainMiddle("mi22",1.0,0.5).chainMiddle("line",1.0)
    //builder.use("mi21").setForgetting(0.02)
    //builder.use("mi22").hush("mi21")
    builder.use("mi22").connect("mi12",-1.0).connect("loop1",-1.0)//.connect("mi11",-1.0)
    builder.use("line").hush("dot").hush("mi12")
    builder.use("O").hush("S").hush("dot")
    builder.use("S").hush("O").hush("line")
    
    build()
    debug("------------")
    net.addAfterFireTrigger("dot", () => println("KROPA!") )
    net.addAfterFireTrigger("line", () => println("KRECHA!") )
    val sb = StringBuilder.newBuilder
    net.addAfterFireTrigger("S", () => {
      println("S!")
      sb.append('S')
    })
 
    net.addAfterFireTrigger("O", () => {
      println("O!")
      sb.append('O'); 
    })
        
    in.tickInterval = Context.sleepTime * 2

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
}