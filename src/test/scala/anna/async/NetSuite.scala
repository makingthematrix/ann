package anna.async

import anna.Context
import anna.async.Messages._
import anna.utils.Utils.await
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite

class NetSuite extends JUnitSuite {
  val threshold = Context().threshold
  val slope = Context().slope
  val weight = Context().weight
  val hushValue = Context().hushValue
  val forgetting = Context().forgetting
  val tickTime = Context().tickTime
  val timeout = Context().timeout

  @Test def shouldCreateNet(){
    val net = NetRef("net1")

    val msg = await[Msg](net.ref, GetId)
    assertEquals("net1",msg.str)
    
    net.shutdown()
  }
  
  @Test def shouldCreateNeurons(){
    val net = NetRef("net1")

    val n1 = net.createNeuron("id1", threshold, slope, hushValue, forgetting, tickTime, ActivationFunction.SIGMOID)
    val n2 = net.createNeuron("id2", threshold, slope, hushValue, forgetting, tickTime, ActivationFunction.SIGMOID)
    
    val msg = await[MsgNeurons](net, GetNeurons)
    val neurons = msg.neurons
    assertEquals(2, neurons.size)
    
    val ids = neurons.map{ _.id }
    assertTrue(ids.contains(n1.id))
    assertTrue(ids.contains(n2.id))
    
    net.shutdown()
  }
  
  @Test def shouldCreateNeuronsWithBuilder(){
    val builder = NetBuilder()
    builder.addMiddle("id1", threshold, slope, hushValue, forgetting)
           .addMiddle("id2", threshold, slope, hushValue, forgetting)
    val net = builder.build("net").net
    
    val neurons = net.getNeurons
    assertEquals(2, neurons.size)
    val ids = neurons.map{ _.id }
    assertTrue(ids.contains("id1"))
    assertTrue(ids.contains("id2"))
    
    net.shutdown()
  }
  
  @Test def shouldConnectNeuronsWithBuilder(){
    val builder = NetBuilder()
    builder.addMiddle("id1", threshold,  slope, hushValue, forgetting)
           .chain("id2", weight, threshold, slope, hushValue, forgetting, 1.0)
    val net = builder.build("net").net
    
    val neurons = net.getNeurons
    assertEquals(2, neurons.size)
    
    val n1Opt = neurons.find(_.id == "id1")
    assertTrue(n1Opt != None)
    val n1 = n1Opt.get
    
    val synapses = n1.getSynapses
    assertEquals(1, synapses.size)
    
    val nRef = synapses(0).dest
    assertEquals("id2", nRef.id)
    
    net.shutdown()
  }

}