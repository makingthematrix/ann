package test.async.epengine

import anna.epengine.{Engine, TossType}
import anna.logger.LOG
import anna.logger.LOG._
import anna.data.{Hush, SynapseWeight}
import anna.utils.DoubleRange._
import org.junit.Assert._
import org.junit.{After, Before, Test}
import org.scalatest.junit.JUnitSuite

class EngineSuite extends JUnitSuite {
  private var engine:Engine = _
  @Before def before(){
    LOG.addLogToStdout()
    engine = new Engine()
    engine.synapseWeightRange = -1.0<=>1.0
    engine.synapseHushProbability = 0.1
    engine.synapsesTossType = TossType.LINEAR
  }
  
  @After def after(){
  }
  
  @Test def shouldTossForSynapse(){
    val totalCount = 1000
    var hushCount = 0
    for(i <- 1 to totalCount){
      val synapseChromosome = engine.tossForSynapse("id1")
      assertEquals("id1",synapseChromosome.neuronId)
      synapseChromosome.weight match {
        case Hush => hushCount = hushCount + 1
        case SynapseWeight(w) => 
          assert(w >= -1.0 && w <= 1.0, s"weight outside range: $w")
      }
    }
    
    debug(this, s"hushCount: $hushCount")
    assertTrue(hushCount > 80)
    assertTrue(hushCount < 120)
  }
  
  @Test def shouldChangeHushProbability(){
    engine.synapseHushProbability = 0.9
    
    val totalCount = 1000
    var hushCount = 0
    for(i <- 1 to totalCount){
      val synapseChromosome = engine.tossForSynapse("id1")
      assertEquals("id1",synapseChromosome.neuronId)
      synapseChromosome.weight match {
        case Hush => hushCount = hushCount + 1
        case SynapseWeight(w) => 
          assert(w >= -1.0 && w <= 1.0, s"weight outside range: $w")
      }
    }
    
    debug(this, s"hushCount: $hushCount")
    assertTrue(hushCount > 800)
  }
}