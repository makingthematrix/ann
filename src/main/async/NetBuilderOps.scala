package main.async

import main.utils.Utils.assert
import main.data.SynapseWeight
import main.async.Context._
import main.data.HushValue
import main.data.ForgetTrait
import main.data.DontForget

class NetBuilderOps(val builder: NetBuilder) extends AnyVal {
  def chainMiddle(id: String, 
                  weight: Double =builder.defWeight, 
                  threshold: Double =builder.defThreshold, 
                  hushValue: HushValue =builder.defHushValue, 
                  forgetting: ForgetTrait =DontForget, 
                  slope: Double =builder.defSlope):NetBuilder =
    builder.chain(id, builder.defMiddleName, weight, threshold, hushValue, forgetting, slope)

  def chain(id: String, 
            weight: Double =builder.defWeight, 
            threshold: Double =builder.defThreshold, 
            hushValue: HushValue =builder.defHushValue, 
            forgetting: ForgetTrait =DontForget, 
            slope: Double =builder.defSlope):NetBuilder =
    chainMiddle(id, weight, threshold, hushValue, forgetting, slope)
  def chainMiddle():NetBuilder = chainMiddle(builder.generateId(MIDDLE_LAYER_NAME))
  def chainMiddle(weight: Double):NetBuilder = chainMiddle(builder.generateId(MIDDLE_LAYER_NAME), weight)
  def chainMiddle(weight: Double, threshold: Double):NetBuilder = 
    chainMiddle(builder.generateId(MIDDLE_LAYER_NAME), weight, threshold)
  def chainMiddle(weight: Double, threshold: Double, slope: Double):NetBuilder = 
    chainMiddle(builder.generateId(MIDDLE_LAYER_NAME), weight, threshold, builder.defHushValue, builder.defForgetting, slope)
    
  def loop(id: String, 
           w1: Double =builder.defWeight, 
           threshold: Double =builder.defThreshold, 
           w2: Double =builder.defWeight, 
           slope: Double =builder.defSlope):NetBuilder = {
    val n1 = builder.current
    if(builder.throwOnError && !builder.contains(n1.id))
      throw new IllegalArgumentException("You can loop only in the middle layer")
    
    chainMiddle(id, w1, threshold, builder.defHushValue, builder.defForgetting, slope)
    
    builder.addSynapse(id, n1.id, SynapseWeight(w2))
    builder.use(n1.id)
    builder
  }
    
  def loop(w1: Double, threshold: Double, w2: Double):NetBuilder = loop(builder.generateId(MIDDLE_LAYER_NAME), w1, threshold, w2)
  def loop(w1: Double, w2: Double):NetBuilder = loop(builder.generateId(MIDDLE_LAYER_NAME), w1, builder.defThreshold, w2)
  def loop():NetBuilder = loop(builder.generateId(MIDDLE_LAYER_NAME))
  
  def oscillator(name: String) = loop(name, 1.0, 0.5, -1.0)
  def oscillator() = loop(1.0, 0.5, -1.0)
  
  def chainOscillator(id: String, weight: Double, threshold: Double) = 
    chainMiddle(id, weight, threshold).oscillator(id+"_osc")
  def chainOscillator(id: String, weight: Double) = chainMiddle(id, weight).oscillator(id+"_osc")
  def chainOscillator(weight: Double) = chainMiddle(weight).oscillator()
  
  def self(weight: Double =builder.defWeight):NetBuilder = {
    builder.addSynapse(builder.current.id, builder.current.id, SynapseWeight(weight))
    builder
  }
  
  implicit private def fromNetBuilder(builder: NetBuilder):NetBuilderOps = NetBuilderOps.fromNetBuilder(builder)
}

object NetBuilderOps {
  implicit def fromNetBuilder(builder: NetBuilder):NetBuilderOps = new NetBuilderOps(builder) 
}