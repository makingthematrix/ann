package anna.async

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import akka.actor._
import anna._
import scala.concurrent._
import scala.concurrent.duration._
import akka.util.Timeout
import anna.async._

import anna.async.logger.LOG
import anna.async.logger.LOG._
import Messages._
import Context._
import anna.utils.Utils.await
import anna.async.NetBuilderOps._

class NetSuite extends JUnitSuite {     
  @Test def shouldCreateNet(){
    val net = NetRef("net1")

    val msg = await[Msg](net.ref, GetId)
    assertEquals("net1",msg.str)
    
    net ! Shutdown
  }
  
  @Test def shouldCreateNeurons(){
    val net = NetRef("net1")

    val n1 = net.createNeuron("id1", threshold, slope, hushValue, forgetting)
    val n2 = net.createNeuron("id2", threshold, slope, hushValue, forgetting)
    
    val msg = await[MsgNeurons](net, GetNeurons)
    val neurons = msg.neurons
    assertEquals(2, neurons.size)
    
    val ids = neurons.map{ _.id }
    assertTrue(ids.contains(n1.id))
    assertTrue(ids.contains(n2.id))
    
    net ! Shutdown
  }
  
  @Test def shouldCreateNeuronsWithBuilder(){
    val builder = NetBuilder()
    builder.addMiddle("id1", threshold, hushValue, forgetting, slope)
           .addMiddle("id2", threshold, hushValue, forgetting, slope)
    val net = builder.build
    
    val neurons = net.getNeurons
    assertEquals(2, neurons.size)
    val ids = neurons.map{ _.id }
    assertTrue(ids.contains("id1"))
    assertTrue(ids.contains("id2"))
    
    net ! Shutdown
  }
  
  @Test def shouldConnectNeuronsWithBuilder(){
    val builder = NetBuilder()
    builder.addMiddle("id1", threshold, hushValue, forgetting, slope)
           .chain("id2", weight, threshold, hushValue, forgetting, slope)
    val net = builder.build
    
    val neurons = net.getNeurons
    assertEquals(2, neurons.size)
    
    val n1Opt = neurons.find(n => n.id == "id1")
    assertTrue(n1Opt != None)
    val n1 = n1Opt.get
    
    val synapses = n1.getSynapses
    assertEquals(1, synapses.size)
    
    val nRef = synapses(0).dest
    assertEquals("id2", nRef.id)
    
    net ! Shutdown
  }

}