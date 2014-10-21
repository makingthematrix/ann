package main.sync

import main.utils.Utils.assert

class NetBuilderOps(builder: NetBuilder) {
  def dotLine(startingPoint: String, dotEnd: String, lineEnd: String, mp: String = "mi") = { 
    //assertions
    assert(builder.middleNeuronType == NeuronType.DELAY, 
           s"The middle neuron type must be DELAY and is ${builder.middleNeuronType}")
    assert(builder.resolution == 4, 
           s"The net resolution must be 4 and is ${builder.resolution}")
    //dot chain
    builder.use(startingPoint).chainMiddle(s"${mp}11",0.28,0.5)
                              .loop(s"${mp}_loop1",1.0,0.5,1.0)
                              .chainMiddle(dotEnd,1.0,0.9)
           .use(s"${mp}_loop1").setPriority(-1)
           .use(s"${mp}11").setForgetting(0.2)
           .use(dotEnd).setForgetting(0.2).setPriority(1000)
                       .connect(s"${mp}11", -0.49)
                       .connect(s"${mp}_loop1", -1.0)
    // line chain
           .use(startingPoint).chainMiddle(s"${mp}21",0.19,0.5)
                              .chainMiddle(lineEnd,1.0,0.5).setPriority(1001)
                              .connect(s"${mp}21", -0.35)
    // if line then not dot
           .use(s"${mp}21").connect(s"${mp}11", -1.0)
                           .connect(s"${mp}_loop1", -1.0)
  }
  
}

object NetBuilderOps{
  implicit def fromNetBuilder(builder: NetBuilder) = new NetBuilderOps(builder) 
}