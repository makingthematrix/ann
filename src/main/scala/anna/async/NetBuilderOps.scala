package anna.async

import anna.data.{ForgetTrait, HushValue}

class NetBuilderOps(val builder: NetBuilder) extends AnyVal {
  private def chainMiddle(id: String,
                  weight: Double =builder.defWeight, 
                  threshold: Double =builder.defThreshold,
                  slope: Double =builder.defSlope,
                  hushValue: HushValue =builder.defHushValue, 
                  forgetting: ForgetTrait =builder.defForgetting,
                  tickTimeMultiplier: Double =builder.defTickTimeMultiplier):NetBuilder =
    builder.chain(id, weight, threshold, slope, hushValue, forgetting, tickTimeMultiplier)

  def chain(id: String, 
            weight: Double,
            threshold: Double,
            slope: Double,
            hushValue: HushValue,
            forgetting: ForgetTrait,
            tickTime: Long):NetBuilder =
    chainMiddle(id, weight, threshold, slope, hushValue, forgetting, tickTime)
  def chain(id: String,
            weight: Double,
            threshold: Double,
            slope: Double,
            hushValue: HushValue,
            forgetting: ForgetTrait):NetBuilder =
    chainMiddle(id, weight, threshold, slope, hushValue, forgetting)
  def chain(id: String,
            weight: Double,
            threshold: Double,
            hushValue: HushValue,
            forgetting: ForgetTrait):NetBuilder =
    chainMiddle(id, weight, threshold, builder.defSlope, hushValue, forgetting)
  def chain(id: String,
            weight: Double,
            threshold: Double,
            hushValue: HushValue):NetBuilder =
    chainMiddle(id, weight, threshold, builder.defSlope, hushValue)
  def chain(id: String,
            weight: Double,
            threshold: Double,
            forgetting: ForgetTrait):NetBuilder =
    chainMiddle(id, weight, threshold, builder.defSlope, builder.defHushValue, forgetting)
  def chain(id: String):NetBuilder = chainMiddle(id)
  def chain(id: String, weight: Double):NetBuilder = chainMiddle(id, weight)
  def chain(id: String, weight: Double, threshold: Double):NetBuilder = chainMiddle(id, weight, threshold)

  def loop(id: String, 
           w1: Double =builder.defWeight, 
           threshold: Double =builder.defThreshold, 
           w2: Double =builder.defWeight, 
           slope: Double =builder.defSlope):NetBuilder = {
    val n1 = builder.current
    if(builder.inputSet.contains(n1.id)) throw new IllegalArgumentException("You can loop only in the middle layer")
    
    chainMiddle(id, w1, threshold, slope, builder.defHushValue, builder.defForgetting)
    
    builder.connect(n1.id, w2).use(n1.id)
  }
    
  def loop(w1: Double, threshold: Double, w2: Double):NetBuilder = loop(builder.generateId(), w1, threshold, w2)
  def loop(w1: Double, w2: Double):NetBuilder = loop(builder.generateId(), w1, builder.defThreshold, w2)
  def loop():NetBuilder = loop(builder.generateId())
  
  def oscillator(name: String) = loop(name, 1.0, 0.5, -1.0)
  def oscillator() = loop(1.0, 0.5, -1.0)
  
  def chainOscillator(id: String, weight: Double, threshold: Double) = 
    chainMiddle(id, weight, threshold).oscillator(id+"_osc")
  def chainOscillator(id: String, weight: Double) = chainMiddle(id, weight).oscillator(id+"_osc")
  def chainOscillator(weight: Double) = chainMiddle(builder.generateId(), weight).oscillator()
  
  def self(weight: Double =builder.defWeight):NetBuilder = builder.connect(builder.current.id, weight)
  
  implicit private def fromNetBuilder(builder: NetBuilder):NetBuilderOps = NetBuilderOps.fromNetBuilder(builder)
}

object NetBuilderOps {
  implicit def fromNetBuilder(builder: NetBuilder):NetBuilderOps = new NetBuilderOps(builder) 
}