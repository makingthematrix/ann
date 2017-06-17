package anna.async

import anna.Context
import anna.data.SynapseWeight
import anna.logger.LOG
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import anna.async.NetBuilderOps._

import scala.concurrent.{Await, Promise}

/**
  * Created by gorywoda on 10/31/16.
  */
class NeuronTriggersSuite extends JUnitSuite {
  val timeout = Context().timeout

  @Test def shouldTriggerAfterFireWithNetWrapper(){
    val wrapper = NetBuilder().addInput("in").chain("out", 1.0, 0.9, 0).build()

    val p = Promise[Boolean]()
    wrapper.addAfterFire("out", "MyTrigger"){
      LOG.debug("received after fire request")
      p.success(true)
    }

    LOG.allow("in", "out")
    wrapper.iterateUntilCalm("1")

    val afterFireReceived = Await.result(p.future, timeout.duration)
    assertTrue(afterFireReceived)

    wrapper.shutdown()
  }

  @Test def shouldTriggerAfterFireWithNeuron(){
    val net = NetRef("net1")

    val n1 = net.createNeuron("id1", 0.0, 0)
    val n2 = net.createNeuron("id2", 0.9, 0)

    n1.setSynapses(List(Synapse(n2, SynapseWeight(1.0))))
    net.setInputs(List(n1.id))

    val p = Promise[Boolean]()

    n2.addAfterFire("MyTrigger"){
      LOG.debug("received after fire in id2")
      p.success(true)
    }

    net.signal(List(1.0))

    val silenceRequestReceived = Await.result(p.future, timeout.duration)
    assertTrue(silenceRequestReceived)

    net.shutdown()
  }

  @Test def shouldTriggerAfterFireWithNet(){
    val net = NetRef("net1")

    val n1 = net.createNeuron("id1", 0.0, 0)
    val n2 = net.createNeuron("id2", 0.9, 0)

    n1.setSynapses(List(Synapse(n2, SynapseWeight(1.0))))
    net.setInputs(List(n1.id))

    val p = Promise[Boolean]()

    net.addAfterFire("id2", "MyTrigger"){
      LOG.debug("received after fire in id2")
      p.success(true)
    }

    LOG.allow("id1", "id2")
    net.signal(List(1.0))

    val silenceRequestReceived = Await.result(p.future, timeout.duration)
    assertTrue(silenceRequestReceived)

    net.shutdown()
  }
}
