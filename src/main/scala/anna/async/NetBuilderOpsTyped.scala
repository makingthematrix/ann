package anna.async

import anna.data._

import scala.language.implicitConversions

/**
 * Typed-Actor version of [[NetBuilderOps]] — the `Double`-weight convenience DSL
 * (`chain`, `loop`, `oscillator`, `self`, `connect`, ...) layered on top of
 * [[NetBuilderTyped]].
 *
 * The `anna.blocks`-based methods of the classic ops (`delayGate`, `signalSum`,
 * `sequencer`) are not ported here, because those blocks build against the
 * classic [[NetBuilder]]; porting them is a separate step.
 */
final class NetBuilderOpsTyped(val builder: NetBuilderTyped) extends AnyVal {
  private def chainMiddle(id: String,
                          weight: SynapseTrait   = builder.defWeight,
                          threshold: Double      = builder.defThreshold,
                          silenceIterations: Int = builder.defSilenceIterations): NetBuilderTyped =
    builder.chain(id, weight, threshold, silenceIterations)

  def chain(id: String,
            weight: Double,
            threshold: Double,
            silenceIterations: Int): NetBuilderTyped =
    chainMiddle(id, SynapseWeight(weight), threshold, silenceIterations)

  def chain(id: String,
            weight: Double,
            threshold: Double): NetBuilderTyped =
    chainMiddle(id, SynapseWeight(weight), threshold, builder.defSilenceIterations)

  def chain(id: String): NetBuilderTyped = chainMiddle(id)
  def chain(id: String, weight: Double): NetBuilderTyped = chainMiddle(id, SynapseWeight(weight))

  def chainHush(id: String, threshold: Double): NetBuilderTyped = chainMiddle(id, Silence(), threshold)

  def loop(id: String,
           w1: SynapseTrait  = builder.defWeight,
           threshold: Double = builder.defThreshold,
           w2: SynapseTrait  = builder.defWeight): NetBuilderTyped = {
    val n1 = builder.current
    if (builder.inputSet.contains(n1.id)) throw new IllegalArgumentException("You can loop only in the middle layer")

    chainMiddle(id, w1, threshold, builder.defSilenceIterations)

    builder.connect(n1.id, w2).use(n1.id)
  }

  def loop(id: String, w1: Double, threshold: Double, w2: Double): NetBuilderTyped =
    loop(id, SynapseWeight(w1), threshold, SynapseWeight(w2))
  def loop(w1: Double, threshold: Double, w2: Double): NetBuilderTyped = loop(builder.generateId(), w1, threshold, w2)
  def loop(w1: Double, w2: Double): NetBuilderTyped = loop(builder.generateId(), w1, builder.defThreshold, w2)
  def loop(): NetBuilderTyped = loop(builder.generateId())

  def oscillator(name: String): NetBuilderTyped = loop(name, 1.0, 0.5, -1.0)
  def oscillator(): NetBuilderTyped = loop(1.0, 0.5, -1.0)

  def chainOscillator(id: String, weight: Double, threshold: Double): NetBuilderTyped =
    chainMiddle(id, SynapseWeight(weight), threshold).oscillator(id + "_osc")
  def chainOscillator(id: String, weight: Double): NetBuilderTyped =
    chainMiddle(id, SynapseWeight(weight)).oscillator(id + "_osc")
  def chainOscillator(weight: Double): NetBuilderTyped =
    chainMiddle(builder.generateId(), SynapseWeight(weight)).oscillator()

  def self(weight: SynapseTrait = builder.defWeight): NetBuilderTyped = builder.connect(builder.current.id, weight)

  def connect(id: String, weight: Double): NetBuilderTyped = builder.connect(id, SynapseWeight(weight))

  implicit private def fromNetBuilder(builder: NetBuilderTyped): NetBuilderOpsTyped =
    NetBuilderOpsTyped.fromNetBuilder(builder)
}

object NetBuilderOpsTyped {
  implicit def fromNetBuilder(builder: NetBuilderTyped): NetBuilderOpsTyped = new NetBuilderOpsTyped(builder)
}
