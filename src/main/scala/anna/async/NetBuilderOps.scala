package anna.async

import anna.blocks.{DelayGate, SignalSum}
import anna.data._

class NetBuilderOps(val builder: NetBuilder) extends AnyVal {
  private def chainMiddle(id: String,
                  weight: SynapseTrait =builder.defWeight,
                  threshold: Double =builder.defThreshold,
                  hushValue: HushValue =builder.defHushValue):NetBuilder =
    builder.chain(id, weight, threshold, hushValue)

  def chain(id: String,
            weight: Double,
            threshold: Double,
            hushValue: HushValue):NetBuilder =
    chainMiddle(id, SynapseWeight(weight), threshold, hushValue)
  def chain(id: String,
            weight: Double,
            threshold: Double):NetBuilder =
    chainMiddle(id, SynapseWeight(weight), threshold, builder.defHushValue)
  def chain(id: String):NetBuilder = chainMiddle(id)
  def chain(id: String, weight: Double):NetBuilder = chainMiddle(id, SynapseWeight(weight))

  def chainHush(id: String, threshold: Double):NetBuilder = chainMiddle(id, Hush(), threshold)

  def loop(id: String, 
           w1: SynapseTrait =builder.defWeight,
           threshold: Double =builder.defThreshold,
           w2: SynapseTrait =builder.defWeight):NetBuilder = {
    val n1 = builder.current
    if(builder.inputSet.contains(n1.id)) throw new IllegalArgumentException("You can loop only in the middle layer")
    
    chainMiddle(id, w1, threshold, builder.defHushValue)
    
    builder.connect(n1.id, w2).use(n1.id)
  }

  def loop(id: String, w1: Double, threshold: Double, w2: Double):NetBuilder =
    loop(id, SynapseWeight(w1), threshold, SynapseWeight(w2))
  def loop(w1: Double, threshold: Double, w2: Double):NetBuilder = loop(builder.generateId(), w1, threshold, w2)
  def loop(w1: Double, w2: Double):NetBuilder = loop(builder.generateId(), w1, builder.defThreshold, w2)
  def loop():NetBuilder = loop(builder.generateId())
  
  def oscillator(name: String) = loop(name, 1.0, 0.5, -1.0)
  def oscillator() = loop(1.0, 0.5, -1.0)
  
  def chainOscillator(id: String, weight: Double, threshold: Double) = 
    chainMiddle(id, SynapseWeight(weight), threshold).oscillator(id+"_osc")
  def chainOscillator(id: String, weight: Double) = chainMiddle(id, SynapseWeight(weight)).oscillator(id+"_osc")
  def chainOscillator(weight: Double) = chainMiddle(builder.generateId(), SynapseWeight(weight)).oscillator()
  
  def self(weight: SynapseTrait =builder.defWeight):NetBuilder = builder.connect(builder.current.id, weight)

  def connect(id: String, weight: Double) = builder.connect(id, SynapseWeight(weight))
  
  implicit private def fromNetBuilder(builder: NetBuilder):NetBuilderOps = NetBuilderOps.fromNetBuilder(builder)

  def delayGate(name: String, delay: Int, inputWeight: Double = 1.0, inputTreshold: Double = 0.0) =
    DelayGate(name, delay).chain(builder, inputWeight, inputTreshold)

  def signalSum(name: String, requiredSignals: Int) = SignalSum(name, requiredSignals).chain(builder)
}

object NetBuilderOps {
  implicit def fromNetBuilder(builder: NetBuilder):NetBuilderOps = new NetBuilderOps(builder) 
}