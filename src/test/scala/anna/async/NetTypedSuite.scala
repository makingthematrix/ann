package anna.async

import akka.actor.typed.Scheduler
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.adapter._
import akka.util.Timeout
import anna.Context
import anna.async.Messages.Msg
import anna.data.SynapseTrait
import org.junit.Assert._
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.Await

/**
 * Typed-Actor counterpart of [[NetSuite]].
 *
 * Each test mirrors the same-named test in `NetSuite`, but drives the typed
 * stack ([[NetRefTyped]] / [[NetTyped]] / [[NeuronRefTyped]] / [[NeuronTyped]],
 * [[SynapseTyped]], [[NetBuilderTyped]]) instead of the classic one.
 */
class NetTypedSuite extends AnyFunSuite {
  private val threshold: Double = Context().threshold
  private val weight: SynapseTrait = Context().weight
  private val silenceIterations: Int = Context().silenceIterations

  private implicit val timeout: Timeout = Context().timeout
  private implicit val scheduler: Scheduler = Context().system.scheduler

  test("shouldCreateNet") {
    val net = NetRefTyped("net1")

    val msg = Await.result(net.ref.ask(NetTyped.GetId(_: akka.actor.typed.ActorRef[Msg])), timeout.duration)
    assertEquals("net1", msg.str)

    net.shutdown()
  }

  test("shouldCreateNeurons") {
    val net = NetRefTyped("net1")

    val n1 = net.createNeuron("id1", threshold, silenceIterations)
    val n2 = net.createNeuron("id2", threshold, silenceIterations)

    val neurons = net.neurons
    assertEquals(2, neurons.size)

    val ids = neurons.map(_.id)
    assertTrue(ids.contains(n1.id))
    assertTrue(ids.contains(n2.id))

    net.shutdown()
  }

  test("shouldCreateNeuronsWithBuilder") {
    val builder = NetBuilderTyped()
    builder.addMiddle("id1", threshold, silenceIterations)
           .addMiddle("id2", threshold, silenceIterations)
    val net = builder.build("net").net

    val neurons = net.neurons
    assertEquals(2, neurons.size)
    val ids = neurons.map(_.id)
    assertTrue(ids.contains("id1"))
    assertTrue(ids.contains("id2"))

    net.shutdown()
  }

  test("shouldConnectNeuronsWithBuilder") {
    val builder = NetBuilderTyped()
    builder.addMiddle("id1", threshold, silenceIterations)
           .chain("id2", weight, threshold, silenceIterations)
    val net = builder.build("net").net

    val neurons = net.neurons
    assertEquals(2, neurons.size)

    val n1Opt = neurons.find(_.id == "id1")
    assertTrue(n1Opt.isDefined)
    val n1 = n1Opt.get

    val synapses = n1.getSynapses
    assertEquals(1, synapses.size)

    val nRef = synapses.head.dest
    assertEquals("id2", nRef.id)

    net.shutdown()
  }
}
