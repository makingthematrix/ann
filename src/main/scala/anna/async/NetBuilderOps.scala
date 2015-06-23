package anna.async

import anna.data._

class NetBuilderOps(val builder: NetBuilder) extends AnyVal {
  private def chainMiddle(id: String,
                  weight: SynapseTrait =builder.defWeight,
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
            tickTimeMultiplier: Double):NetBuilder =
    chainMiddle(id, SynapseWeight(weight), threshold, slope, hushValue, forgetting, tickTimeMultiplier)
  def chain(id: String,
            weight: Double,
            threshold: Double,
            slope: Double,
            hushValue: HushValue,
            forgetting: ForgetTrait):NetBuilder =
    chainMiddle(id, SynapseWeight(weight), threshold, slope, hushValue, forgetting)
  def chain(id: String,
            weight: Double,
            threshold: Double,
            hushValue: HushValue,
            forgetting: ForgetTrait):NetBuilder =
    chainMiddle(id, SynapseWeight(weight), threshold, builder.defSlope, hushValue, forgetting)
  def chain(id: String,
            weight: Double,
            threshold: Double,
            hushValue: HushValue):NetBuilder =
    chainMiddle(id, SynapseWeight(weight), threshold, builder.defSlope, hushValue)
  def chain(id: String,
            weight: Double,
            threshold: Double,
            forgetting: ForgetTrait):NetBuilder =
    chainMiddle(id, SynapseWeight(weight), threshold, builder.defSlope, builder.defHushValue, forgetting)
  def chain(id: String):NetBuilder = chainMiddle(id)
  def chain(id: String, weight: Double):NetBuilder = chainMiddle(id, SynapseWeight(weight))
  def chain(id: String, weight: Double, threshold: Double):NetBuilder = chainMiddle(id, SynapseWeight(weight), threshold)

  def chainHush(id: String, threshold: Double):NetBuilder = chainMiddle(id, Hush(), threshold)

  def loop(id: String, 
           w1: SynapseTrait =builder.defWeight,
           threshold: Double =builder.defThreshold,
           w2: SynapseTrait =builder.defWeight,
           slope: Double =builder.defSlope):NetBuilder = {
    val n1 = builder.current
    if(builder.inputSet.contains(n1.id)) throw new IllegalArgumentException("You can loop only in the middle layer")
    
    chainMiddle(id, w1, threshold, slope, builder.defHushValue, builder.defForgetting)
    
    builder.connect(n1.id, w2).use(n1.id)
  }

  def loop(id: String, w1: Double, threshold: Double, w2: Double, slope: Double):NetBuilder =
    loop(id, SynapseWeight(w1), threshold, SynapseWeight(w2), slope)
  def loop(id: String, w1: Double, threshold: Double, w2: Double):NetBuilder =
    loop(id, SynapseWeight(w1), threshold, SynapseWeight(w2), builder.defSlope)
  def loop(w1: Double, threshold: Double, w2: Double):NetBuilder = loop(builder.generateId(), w1, threshold, w2, builder.defSlope)
  def loop(w1: Double, w2: Double):NetBuilder = loop(builder.generateId(), w1, builder.defThreshold, w2, builder.defSlope)
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
    val itm = 3.0
    builder.inputTickMultiplier = itm
    builder.defSlope = 5.0
    builder.addInput("in")
    // dots
    builder.use("in").chain("mi11",1.0,0.0,HushValue((2 * itm).toInt)).hush("mi11")
      .chain("mi12",1.0,0.0).loop("loop",1.0,0.0,1.0)
      .chain("dot",0.6/(2.0*itm),0.6)
      .chain("S",0.5,0.81)
    builder.addHushNeuron("dot_hush").hush("mi12").hush("loop").hush("dot")
    builder.use("dot").hush("dot_hush")

    // lines
    builder.use("in").chain("mi21",0.55,0.58,HushValue(),ForgetValue(0.4 / itm)).hush("mi21")
      .chain("line",1.0,0.0).hush("line")
      .chain("O",0.6,0.81)

    // if line then not dot
    builder.use("line").hush("dot_hush")

    // if S then not O, if O then not S...
    builder.use("S").chainHushNeuron("hush_letters").hush("S").hush("O")
    builder.use("O").hush("hush_letters")

    builder.setName(name).data
  }

  def SOSNetWithHushNeuron() = {
    SOSNetData()
    val netWrapper = builder.build("net")

    netWrapper.addAfterFire("in")( (_:Double)=>{ println("INCOMING!") } )
    netWrapper.addAfterFire("dot")( (_:Double)=>{ println("KROPA!") } )
    netWrapper.addAfterFire("line")( (_:Double)=>{ println("KRECHA!") } )

    netWrapper
  }
}

object NetBuilderOps {
  implicit def fromNetBuilder(builder: NetBuilder):NetBuilderOps = new NetBuilderOps(builder) 
}