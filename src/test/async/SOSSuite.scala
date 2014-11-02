package test.async

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import main.async.Messages._
import main.logger.LOG.debug
import main.logger.LOG
import main.async.Context.sleepTime

class SOSSuite extends MySuite {
  val s = "1,0,0,1,0,0,1,0,0"
  val o = "1,1,0,1,1,0,1,1,0"
      
  private lazy val sosNet = {
    builder.defSlope = 5.0
    builder.resolution = 4
    
    builder.addInput("in1").chainMiddle("mi11",0.24,0.5).loop("loop1",0.75,0.5,1.0).chainMiddle("mi12",1.0,0.75).chainMiddle("dot",1.0)
    builder.use("dot").hush("mi11").hush("mi12").hush("loop1")
           .chainMiddle("S",0.5,0.9).setForgetting(0.01)
           
    builder.use("in1").chainMiddle("mi21",0.13,0.5).chainMiddle("mi22",1.0,0.5).chainMiddle("line",1.0)
    builder.use("mi21").setForgetting(0.02)
    builder.use("mi22").hush("mi21")
    builder.use("line").chainMiddle("O",0.9,0.9).setForgetting(0.01)
    
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
    
    sb
  }
    
  @Test def shouldReturnS() = {
    val sb = sosNet
    LOG.allow("dot","S")
    in += s
    init()
    val interval = in.tickUntilCalm()
    assertEquals("S",sb.toString)
    println(s"interval: $interval")
    LOG.clearAllowedIds()
  }
  
  @Test def shouldReturnO() = {
    val sb = sosNet
    LOG.allow("line","O")
    in += o
    init()
    val interval = in.tickUntilCalm()
    assertEquals("O",sb.toString)
    println(s"interval: $interval")
    LOG.clearAllowedIds()
  }
}