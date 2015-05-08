package anna.async

import anna.Context
import anna.async.NetBuilderOps._
import anna.data.Hush
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test

import scala.concurrent.{Await, Promise}

class HushSuite extends MySuite {
  val threshold = Context().threshold
  val slope = Context().slope
  val hushValue = Context().hushValue
  val forgetting = Context().forgetting
  val tickTime = Context().tickTime
  val timeout = Context().timeout

  @Test def shouldSendHush(){
    val net = NetRef("net1")

    val n1 = net.createNeuron("id1", threshold, slope, hushValue, forgetting, tickTime, ActivationFunction.SIGMOID)
    val n2 = net.createNeuron("id2", threshold, slope, hushValue, forgetting, tickTime, ActivationFunction.SIGMOID)
    
    n1.setSynapses(List(Synapse(n2,Hush())))
    net.setInputs(List(n1.id))
    
    val p = Promise[Boolean]()

    n2.addHushRequested("id2_hush"){
      LOG.debug("received hush request in id2")
      p.success(true)
    }
    
    net.signal(List(1.0))
    
    val hushReceived = Await.result(p.future, timeout.duration)
    assertTrue(hushReceived)
  }
  
  @Test def shouldSendHushThroughBuilder(){
    builder.addInput("id1").addMiddle("id2")
    builder.use("id1").hush("id2")
    build()
    
    var hushReceived = false

    netWrapper.addHushRequested("id2"){
      LOG.debug("received hush request in id2")
      hushReceived = true
    }

    netWrapper += "1"

    LOG.timer()

    netWrapper.tickUntilCalm()
    assertTrue(hushReceived)    
  }
  
  @Test def shouldUseHushNeuron(){
    val net = NetRef("net1")

    val n1 = net.createNeuron("id1", threshold, slope, hushValue, forgetting, tickTime, ActivationFunction.SIGMOID)
    val hushNeuron = net.createHushNeuron("hushneuron")
    val n2 = net.createNeuron("id2", threshold, slope, hushValue, forgetting, tickTime, ActivationFunction.SIGMOID)
    
    n1.setSynapses(List(Synapse(hushNeuron)))
    hushNeuron.setSynapses(List(Synapse(n2)))
    net.setInputs(List(n1.id))
    
    val p = Promise[Boolean]()

    n2.addHushRequested("id2_hush"){
      LOG.debug("received hush request in id2")
      p.success(true)
    }
    
    net.signal(List(1.0))
    
    val hushReceived = Await.result(p.future, timeout.duration)
    assertTrue(hushReceived)
  }
  
  
  @Test def shouldUseHushNeuronWithBuilder(){
    builder.addInput("id1").chainHushNeuron("hushneuron").chain("id2")
    build()
    
    var hushReceived = false

    netWrapper.addHushRequested("id2"){
      LOG.debug("received hush request in id2")
      hushReceived = true
    }

    netWrapper += "1"
     
    LOG.timer()

    netWrapper.tickUntilCalm()
    assertTrue(hushReceived)
  }
}