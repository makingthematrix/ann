package anna.async

import akka.util.Timeout
import anna.Context
import anna.data.SynapseWeight
import anna.logger.LOG
import org.junit.Assert._
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.{Await, Promise}

/**
 * Typed-Actor counterpart of [[NeuronTriggersSuite]].
 *
 * Each test mirrors the same-named test in `NeuronTriggersSuite`, driving the
 * typed stack ([[NetBuilderTyped]] / [[NetWrapperTyped]] / [[NetRefTyped]] /
 * [[NeuronRefTyped]], [[SynapseTyped]]).
 *
 * Note: the classic suite uses the `Double`-weight `chain` overload from
 * `NetBuilderOps` (not ported). Here we call `NetBuilderTyped.chain` with an
 * explicit `SynapseWeight`, which is equivalent.
 */
class NeuronTriggersTypedSuite extends AnyFunSuite {
  private val timeout: Timeout = Context().timeout

  test("shouldTriggerAfterFireWithNetWrapper") {
    val wrapper = NetBuilderTyped().addInput("in").chain("out", SynapseWeight(1.0), 0.9, 0).build()

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
    val net = NetRefTyped("net1")

    val n1 = net.createNeuron("id1", 0.0, 0)
    val n2 = net.createNeuron("id2", 0.9, 0)

    n1.setSynapses(List(SynapseTyped(n2, SynapseWeight(1.0))))
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
    val net = NetRefTyped("net1")

    val n1 = net.createNeuron("id1", 0.0, 0)
    val n2 = net.createNeuron("id2", 0.9, 0)

    n1.setSynapses(List(SynapseTyped(n2, SynapseWeight(1.0))))
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
