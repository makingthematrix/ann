package test.async

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import akka.actor._
import main._
import scala.concurrent._
import scala.concurrent.duration._
import akka.util.Timeout
import main.async._

import main.async.logger.LOG
import main.async.logger.LOG._
import Messages._
import Context._
import main.utils.Utils.await

class NetSuite extends JUnitSuite {     
  @Test
  def shouldCreateNet(){
    val net = NetRef("net1")

    val msg = await[Msg](net.ref, GetId)
    assertEquals("net1",msg.str)
    
    net ! Shutdown
  }
  
  @Test
  def shouldCreateNeurons(){
    val net = NetRef("net1")

    val n1 = net.createNeuron("id1", threshold, slope, hushValue, forgetting)
    val n2 = net.createNeuron("id2", threshold, slope, hushValue, forgetting)
    
    net.init()
    
    val msg = await[MsgNeurons](net, GetNeurons)
    val neurons = msg.neurons
    assertEquals(2, neurons.size)
    assertTrue(neurons.map{ _.id }.contains(n1.id))
    assertTrue(neurons.map{ _.id }.contains(n2.id))
    
    net ! Shutdown
  }
  
  @Test
  def shouldInitializeTheNet(){
    LOG.addLogToStdout()
    val builder = NetBuilder()
    builder.addInput("in").chain("mi").loop().chain("out")
    
    val net = builder.build
    
    debug("--------------------------------")
    assertTrue(net.init())
    debug("--------------------------------")
  }

}