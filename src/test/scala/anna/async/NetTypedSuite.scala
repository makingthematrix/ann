package anna.async

import akka.util.Timeout
import anna.Context
import anna.data.SynapseWeight
import anna.logger.LOG
import org.junit.Assert._
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.{Await, Promise}

/**
 * Smoke tests for the typed-Actor stack ([[NetRefTyped]], [[NetTyped]],
 * [[NeuronRefTyped]], [[NeuronTyped]]).
 *
 * These exercise the typed spawn + `AskPattern` request/response + fire path.
 * They deliberately avoid inter-neuron synapses, because [[Synapse]] still
 * points at the untyped [[NeuronRef]] (the synapse layer is not migrated yet).
 */
class NetTypedSuite extends AnyFunSuite {
  private val timeout: Timeout = Context().timeout

  test("typed: create neurons and query them via AskPattern") {
    val net = NetRefTyped("typed-query")

    net.createNeuron("a", 0.5, 0)
    net.createNeuron("b", 0.9, 0)

    assertEquals(Set("a", "b"), net.neuronsIds.toSet)
    assertEquals(2, net.neuronsSize)

    val b = net.find("b")
    assertTrue(b.isDefined)
    assertEquals(0.9, b.get.info.threshold, 0.0001)
    assertTrue(net.find("nope").isEmpty)

    net.shutdown()
  }

  test("typed: set inputs, signal and fire triggers an after-fire callback") {
    val net = NetRefTyped("typed-fire")

    val in = net.createNeuron("in", 0.9, 0)
    net.setInputs(List("in"))

    val p = Promise[Boolean]()
    in.addAfterFire("MyTrigger") {
      LOG.debug("typed neuron fired")
      p.success(true)
    }

    net.signal(List(1.0))

    assertTrue(Await.result(p.future, timeout.duration))
    assertEquals(1L, net.iteration)

    net.shutdown()
  }

  test("typed: signal propagates through a SynapseTyped chain and fires downstream") {
    val net = NetRefTyped("typed-chain")

    val n1 = net.createNeuron("id1", 0.0, 0) // fires on any signal
    val n2 = net.createNeuron("id2", 0.9, 0)

    n1.setSynapses(List(SynapseTyped(n2, SynapseWeight(1.0))))
    net.setInputs(List("id1"))

    val p = Promise[Boolean]()
    n2.addAfterFire("MyTrigger") {
      LOG.debug("downstream typed neuron fired")
      p.success(true)
    }

    net.signal(List(1.0))

    assertTrue(Await.result(p.future, timeout.duration))
    assertEquals(1, n1.getSynapses.size)

    net.shutdown()
  }

  test("typed: NetBuilderTyped builds a net driven by NetWrapperTyped") {
    val wrapper = NetBuilderTyped("typed-builder")
      .addInput("in")
      .chain("out", SynapseWeight(1.0), 0.9, 0)
      .build()

    assertEquals(Seq("in"), wrapper.ids)

    val p = Promise[Boolean]()
    wrapper.addAfterFire("out", "MyTrigger") {
      LOG.debug("built typed net fired")
      p.success(true)
    }

    wrapper.iterateUntilCalm("1")

    assertTrue(Await.result(p.future, timeout.duration))

    wrapper.shutdown()
  }

  test("typed: shutdown replies with NetShutdownDone") {
    val net = NetRefTyped("typed-shutdown")
    net.createNeuron("x", 0.5, 0)

    val done = net.shutdown()
    assertEquals("typed-shutdown", done.id)
  }
}
