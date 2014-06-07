package test

import org.scalatest.junit.JUnitSuite

import org.junit.{Test, Before}
import org.junit.Assert._
import main._
import Utils._

class SOSSuite extends JUnitSuite {
  // i'm still not sure if it's a good idea to use implicit classes for it...
  implicit class DotLineNetBuilder(builder: NetBuilder){
    def dotLine(startingPoint: String, mp: String, dotEnd: String, lineEnd: String) = { 
      // startingPoint must exist, ending points may or may not exist
      // mp: middle prefix, a prefix for all inner neurons so they won't get confused with any others
      
      //assertions
      assert(builder.middleNeuronType == NeuronType.DELAY, s"The middle neuron type must be DELAY and is ${builder.middleNeuronType}")
      assert(builder.resolution == 4, s"The net resolution must be 4 and is ${builder.resolution}")
      
      builder.throwOnError = false
      
      //dot chain
      builder.use(startingPoint)
             .chainMiddle(s"${mp}11",0.28,0.5)
             .loop(s"${mp}_loop1",1.0,0.5,1.0)
             .chainMiddle(s"${mp}12",1.0,0.9)
             .chainOutput(dotEnd,1.0)
      builder.use(s"${mp}11").setForgetting(0.2)
      builder.use(s"${mp}12").setForgetting(0.2)
      builder.use(s"${mp}12").connect(s"${mp}11", -0.49)
      builder.use(s"${mp}12").connect(s"${mp}_loop1", -1.0)
      builder.use(dotEnd).connect(s"${mp}12", -1.0)
      // line chain
      builder.use(startingPoint)
             .chainMiddle(s"${mp}21",0.19,0.5)
             .chainMiddle(s"${mp}22",1.0,0.5)
             .chainOutput(lineEnd,1.0)
      builder.use(s"${mp}22").connect(s"${mp}21", -0.35)
      // if line then not dot
      builder.use(s"${mp}21").connect(s"${mp}11", -1.0)
      builder.use(s"${mp}21").connect(s"${mp}_loop1", -1.0)
      
      builder.throwOnError = true
      
      builder
    }
  }
  
  private lazy val sosNet = {
    val builder = NetBuilder(NeuronType.DELAY, 5.0, 0.1, 4)

    builder.addInput("in1")
   
    builder.dotLine("in1","mi","out1","out2")
    
    val (in, net, out) = builder.build("in","out")
    val out1 = builder.get("out1")
    val sb = StringBuilder.newBuilder
    out.addAfterFireTrigger(out1, (n:Neuron) => {
      println("KROPA!")
      sb.append('.'); 
    })
    val out2 = builder.get("out2")
    out.addAfterFireTrigger(out2, (n:Neuron) => {
      println("KRECHA!")
      sb.append('-'); 
    })
    
    (in, sb, net)
  }
  
}