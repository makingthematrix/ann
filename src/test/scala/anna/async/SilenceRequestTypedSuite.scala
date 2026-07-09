package anna.async

import akka.util.Timeout
import anna.Context
import anna.async.NetBuilderOpsTyped._
import anna.data.Silence
import anna.logger.LOG
import org.junit.Assert._
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.{Await, Promise}

/**
 * Typed-Actor counterpart of [[SilenceRequestSuite]].
 *
 * Each test mirrors the same-named test in `SilenceRequestSuite`, driving the
 * typed stack ([[NetRefTyped]] / [[NeuronRefTyped]] / [[SynapseTyped]] /
 * [[SilencingNeuronTyped]], [[NetBuilderTyped]] + [[NetBuilderOpsTyped]] /
 * [[NetWrapperTyped]]).
 */
class SilenceRequestTypedSuite extends AnyFunSuite {

  private val threshold: Double = Context().threshold
  private val silenceIterations: Int = Context().silenceIterations
  private val timeout: Timeout = Context().timeout

  test("shouldSendSilenceRequest") {
    val net = NetRefTyped("net1")

    val n1 = net.createNeuron("id1", threshold, silenceIterations)
    val n2 = net.createNeuron("id2", threshold, silenceIterations)

    n1.setSynapses(List(SynapseTyped(n2, Silence())))
    net.setInputs(List(n1.id))

    val p = Promise[Boolean]()

    n2.addSilenceRequested("id2_silence") {
      LOG.debug("received silence request in id2")
      p.success(true)
    }

    net.signal(List(1.0))

    val silenceRequestReceived = Await.result(p.future, timeout.duration)
    assertTrue(silenceRequestReceived)

    net.shutdown()
  }

  test("shouldSendSilenceRequestThroughBuilder") {
    val netWrapper = NetBuilderTyped().addInput("id1").addMiddle("id2")
                                      .use("id1").silence("id2").build()

    var silenceRequestReceived = false

    netWrapper.addSilenceRequested("id2") {
      LOG.debug("received silence request in id2")
      silenceRequestReceived = true
    }

    netWrapper += "1"

    netWrapper.iterateUntilCalm()
    assertTrue(silenceRequestReceived)

    netWrapper.shutdown()
  }

  test("shouldUseSilencingNeuron") {
    val net = NetRefTyped("net1")

    val n1 = net.createNeuron("id1", threshold, silenceIterations)
    val silencingNeuron = net.createSilencingNeuron("silencingneuron")
    val n2 = net.createNeuron("id2", threshold, silenceIterations)

    n1.setSynapses(List(SynapseTyped(silencingNeuron)))
    silencingNeuron.setSynapses(List(SynapseTyped(n2)))
    net.setInputs(List(n1.id))

    val p = Promise[Boolean]()

    n2.addSilenceRequested("id2_silence") {
      LOG.debug("received silence request in id2")
      p.success(true)
    }

    net.signal(List(1.0))

    val silenceRequestReceived = Await.result(p.future, timeout.duration)
    assertTrue(silenceRequestReceived)

    net.shutdown()
  }

  test("shouldUseSilencingNeuronWithBuilder") {
    val netWrapper = NetBuilderTyped().addInput("id1").chainSilencingNeuron("silencingneuron").chain("id2").build()

    var silenceRequestReceived = false

    netWrapper.addSilenceRequested("id2") {
      LOG.debug("received silence request in id2")
      silenceRequestReceived = true
    }

    netWrapper += "1"

    netWrapper.iterateUntilCalm()
    assertTrue(silenceRequestReceived)

    netWrapper.shutdown()
  }
}
