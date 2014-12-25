package anna.async

import org.junit.Test
import anna.async.NetRef
import anna.async.Context._
import anna.async.Synapse
import anna.data.Hush
import org.junit.Assert._
import anna.async.logger.LOG
import scala.concurrent.Promise
import scala.concurrent.Future
import scala.concurrent.Await
import anna.async.Messages._
import anna.async.NetBuilderOps._

class HushSuite extends MySuite {
  @Test def shouldSendHush(){
    val net = NetRef("net1")

    val n1 = net.createNeuron("id1", threshold, slope, hushValue, forgetting)
    val n2 = net.createNeuron("id2", threshold, slope, hushValue, forgetting)
    
    n1.setSynapses(List(Synapse(n2,Hush)))
    net.setInputLayer(List(n1.id))
    
    val p = Promise[Boolean]()

    n2.addHushRequested("id2_hush"){
      LOG.debug("received hush request in id2")
      p.success(true)
    }
    
    net.signal(List(1.0))
    
    val hushReceived = Await.result(p.future, timeout.duration).asInstanceOf[Boolean]
    assertTrue(hushReceived)
    
    net.shutdown()
  }
  
  @Test def shouldSendHushThroughBuilder(){
    builder.addInput("id1").addMiddle("id2")
    builder.use("id1").hush("id2")
    build()
    
    var hushReceived = false

    net.addHushRequested("id2"){
      LOG.debug("received hush request in id2")
      hushReceived = true
    }
    
    in += "1"

    LOG.timer()
    
    in.tickUntilCalm()
    assertTrue(hushReceived)    
  }
  
  @Test def shouldUseHushNeuron(){
    val net = NetRef("net1")

    val n1 = net.createNeuron("id1", threshold, slope, hushValue, forgetting)
    val hushNeuron = net.createHushNeuron("hushneuron")
    val n2 = net.createNeuron("id2", threshold, slope, hushValue, forgetting)
    
    n1.setSynapses(List(Synapse(hushNeuron)))
    hushNeuron.setSynapses(List(Synapse(n2)))
    net.setInputLayer(List(n1.id))
    
    val p = Promise[Boolean]()

    n2.addHushRequested("id2_hush"){
      LOG.debug("received hush request in id2")
      p.success(true)
    }
    
    net.signal(List(1.0))
    
    val hushReceived = Await.result(p.future, timeout.duration).asInstanceOf[Boolean]
    assertTrue(hushReceived)
    
    net.shutdown()
  }
  
  
  @Test def shouldUseHushNeuronWithBuilder(){
    builder.addInput("id1").chainHushNeuron("hushneuron").chain("id2")
    build()
    
    var hushReceived = false

    net.addHushRequested("id2"){
      LOG.debug("received hush request in id2")
      hushReceived = true
    }
    
    in += "1"
     
    LOG.timer()
    
    in.tickUntilCalm()
    assertTrue(hushReceived)
  }
}