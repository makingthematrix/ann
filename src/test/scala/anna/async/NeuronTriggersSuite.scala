package anna.async

import akka.util.Timeout
import anna.Context
import anna.data.SynapseWeight
import anna.logger.LOG
import org.junit.Assert._
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.{Await, Promise}

/**
 * Tests for the neuron fire/silence triggers, exercised through [[NetBuilder]]
 * / [[NetWrapper]] and directly through [[NetRef]] / [[NeuronRef]] / [[Synapse]].
 */
class NeuronTriggersSuite extends AnyFunSuite {
  private val timeout: Timeout = Context().timeout

  test("shouldTriggerAfterFireWithNetWrapper") {
    val wrapper = NetBuilder().addInput("in").chain("out", SynapseWeight(1.0), 0.9, 0).build()

    val p = Promise[Boolean]()
    wrapper.addAfterFire("out", "MyTrigger") {
      LOG.debug("received after fire request")
      p.success(true)
    }

    LOG.allow("in", "out")
    wrapper.iterateUntilCalm("1")

    val afterFireReceived = Await.result(p.future, timeout.duration)
    assertTrue(afterFireReceived)

    wrapper.shutdown()
  }

  test("shouldTriggerAfterFireWithNeuron") {
    val net = NetRef("net1")

    val n1 = net.createNeuron("id1", 0.0, 0)
    val n2 = net.createNeuron("id2", 0.9, 0)

    n1.setSynapses(List(Synapse(n2, SynapseWeight(1.0))))
    net.setInputs(List(n1.id))

    val p = Promise[Boolean]()

    n2.addAfterFire("MyTrigger") {
      LOG.debug("received after fire in id2")
      p.success(true)
    }

    net.signal(List(1.0))

    val afterFireReceived = Await.result(p.future, timeout.duration)
    assertTrue(afterFireReceived)

    net.shutdown()
  }

  test("shouldTriggerAfterFireWithNet") {
    val net = NetRef("net1")

    val n1 = net.createNeuron("id1", 0.0, 0)
    val n2 = net.createNeuron("id2", 0.9, 0)

    n1.setSynapses(List(Synapse(n2, SynapseWeight(1.0))))
    net.setInputs(List(n1.id))

    val p = Promise[Boolean]()

    net.addAfterFire("id2", "MyTrigger") {
      LOG.debug("received after fire in id2")
      p.success(true)
    }

    LOG.allow("id1", "id2")
    net.signal(List(1.0))

    val afterFireReceived = Await.result(p.future, timeout.duration)
    assertTrue(afterFireReceived)

    net.shutdown()
  }
}
