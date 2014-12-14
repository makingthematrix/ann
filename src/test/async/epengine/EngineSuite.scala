package test.async.epengine

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import main.async.Hush
import main.async.SynapseWeight
import main.async.epengine.Engine
import main.utils.DoubleRange
import main.utils.DoubleRange._
import main.async.epengine.TossType
import main.async.logger.LOG._
import main.async.logger.LOG
import org.junit.Before
import org.junit.After

class EngineSuite extends JUnitSuite {
  @Before def before(){
    LOG.addLogToStdout()
  }
  
  @After def after(){
  }
  
  @Test def shouldTossForSynapse(){
    val engine = new Engine()
    engine.synapseWeightRange = -1.0<=>1.0
    engine.synapseHushProbability = 0.1
    engine.synapsesTossType = TossType.LINEAR
    
    val totalCount = 1000
    var hushCount = 0
    for(i <- 1 to totalCount){
      val synapseChromosome = engine.tossForSynapse("id1")
      assertEquals("id1",synapseChromosome.id)
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
}