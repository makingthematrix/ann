package anna.async

import akka.util.Timeout
import anna.Context
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

  test("typed: shutdown replies with NetShutdownDone") {
    val net = NetRefTyped("typed-shutdown")
    net.createNeuron("x", 0.5, 0)

    val done = net.shutdown()
    assertEquals("typed-shutdown", done.id)
  }
}
