package anna.async

import anna.data._
import anna.epengine.{DelayGate, DelayGateWithBreak, SignalSum, ConstantCurrent}

class NetBuilderOps(val builder: NetBuilder) extends AnyVal {
  private def chainMiddle(id: String,
                  weight: SynapseTrait =builder.defWeight,
                  threshold: Double =builder.defThreshold,
                  hushValue: HushValue =builder.defHushValue, 
                  forgetting: ForgetTrait =builder.defForgetting):NetBuilder =
    builder.chain(id, weight, threshold, hushValue, forgetting)

  def chain(id: String,
            weight: Double,
            threshold: Double,
            hushValue: HushValue,
            forgetting: ForgetTrait):NetBuilder =
    chainMiddle(id, SynapseWeight(weight), threshold, hushValue, forgetting)
  def chain(id: String,
            weight: Double,
            threshold: Double,
            hushValue: HushValue):NetBuilder =
    chainMiddle(id, SynapseWeight(weight), threshold, hushValue)
  def chain(id: String,
            weight: Double,
            threshold: Double,
            forgetting: ForgetTrait):NetBuilder =
    chainMiddle(id, SynapseWeight(weight), threshold, builder.defHushValue, forgetting)
  def chain(id: String):NetBuilder = chainMiddle(id)
  def chain(id: String, weight: Double):NetBuilder = chainMiddle(id, SynapseWeight(weight))
  def chain(id: String, weight: Double, threshold: Double):NetBuilder = chainMiddle(id, SynapseWeight(weight), threshold)

  def chainHush(id: String, threshold: Double):NetBuilder = chainMiddle(id, Hush(), threshold)

  def loop(id: String, 
           w1: SynapseTrait =builder.defWeight,
           threshold: Double =builder.defThreshold,
           w2: SynapseTrait =builder.defWeight):NetBuilder = {
    val n1 = builder.current
    if(builder.inputSet.contains(n1.id)) throw new IllegalArgumentException("You can loop only in the middle layer")
    
    chainMiddle(id, w1, threshold, builder.defHushValue, builder.defForgetting)
    
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

  def SOSNetData(name: String = "net") = {
    builder.addInput("in")
    // dots
    builder.use("in").chain("mi11",1.0,0.0,HushValue(2)).hush("mi11")
      .chain("mi12",1.0,0.0).loop("loop",1.0,0.0,1.0)
      .chain("dot",0.6/2.0,0.6)
      .chain("S",0.5,0.81)
    builder.addHushNeuron("dot_hush").hush("mi12").hush("loop").hush("dot")
    builder.use("dot").hush("dot_hush")

    // lines
    builder.use("in").chain("mi21",0.55,0.58,HushValue(),ForgetValue(0.4)).hush("mi21")
      .chain("line",1.0,0.0).hush("line")
      .chain("O",0.6,0.81)

    // if line then not dot
    builder.use("line").hush("dot_hush")

    // if S then not O, if O then not S...
    builder.use("S").chainHushNeuron("hush_letters").hush("S").hush("O")
    builder.use("O").hush("hush_letters")

    builder.netId(name).data
  }

  def SOSNetWithHushNeuron() = {
    SOSNetData()
    val netWrapper = builder.build("net")

    netWrapper.addAfterFire("in")( (_:Double)=>{ println("INCOMING!") } )
    netWrapper.addAfterFire("dot")( (_:Double)=>{ println("KROPA!") } )
    netWrapper.addAfterFire("line")( (_:Double)=>{ println("KRECHA!") } )

    netWrapper
  }

  def SOSNetTemplateData(name: String = "net") = {
    builder.addInput("in").chain("dot",1.0,0.5).chain("S",1.0,0.5)
               .use("in").chain("line",1.0,0.5).chain("O",1.0,0.5)
               .use("dot").connect("O",0.5)
               .use("line").connect("S",0.5)
               .netId(name).data

  }

  def delayGate(name: String, delay: Int, inputWeight: Double = 1.0, inputTreshold: Double = 0.0) =
    DelayGate(name, delay).chain(builder, inputWeight, inputTreshold)

  def delayGateWithBreak(name: String, delay: Int, inputWeight: Double = 1.0, inputTreshold: Double = 0.0) =
    DelayGateWithBreak(name, delay).chain(builder, inputWeight, inputTreshold)

  def signalSum(name: String, requiredSignals: Int) = SignalSum(name, requiredSignals).chain(builder)

  def constantCurrent(name: String, requiredSignals: Int) = ConstantCurrent(name, requiredSignals).chain(builder)
}

object NetBuilderOps {
  implicit def fromNetBuilder(builder: NetBuilder):NetBuilderOps = new NetBuilderOps(builder) 
}