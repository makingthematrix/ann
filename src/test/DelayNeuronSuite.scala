package test

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import main.DelayNeuron

class DelayNeuronSuite extends JUnitSuite{
  val SLOPE = 20.0
  val TRESHOLD = 0.5
  
  @Test
  def shouldCreateNeuron(){
    val i = DelayNeuron.getSerialId
    val n = DelayNeuron(TRESHOLD, SLOPE)
    assertEquals(SLOPE,n.slope,0.01)
    assertEquals(TRESHOLD,n.treshold,0.01)
    assertEquals("delay_"+i,n.id)
  }
  
  @Test
  def shouldConnect(){
    val n1 = DelayNeuron(TRESHOLD, SLOPE)
    val n2 = DelayNeuron(TRESHOLD, SLOPE)
    n1.connect(n2,1.0)
    val s = n1.findSynapse(n2)
    assertFalse(s == None)
  }
  
  @Test
  def shouldGetOutput(){
    val n = DelayNeuron(TRESHOLD, SLOPE)
    n += (TRESHOLD + 0.1)
    n.tick()
    n.tick()
    assertTrue(n.lastOutput > 0.0)
  }
  
  @Test
  def shouldSendSignal(){
    val n1 = DelayNeuron(TRESHOLD, SLOPE)
    val n2 = DelayNeuron(TRESHOLD, SLOPE)
    n1.connect(n2,1.0)
    n1 += (TRESHOLD + 0.1)

    assertEquals(TRESHOLD + 0.1, n1.input,0.01)
    assertEquals(0.0, n1.getLastTickBuffer,0.01)
    assertEquals(0.0, n1.lastOutput,0.01)
    
    n1.tick()
    
    assertEquals(TRESHOLD + 0.1, n1.input,0.01)
    assertEquals(TRESHOLD + 0.1, n1.getLastTickBuffer,0.01)
    assertEquals(0.0, n1.lastOutput,0.01)
    
    n1.tick()
    
    assertEquals(0.0, n1.input,0.01)
    assertEquals(0.0, n1.getLastTickBuffer,0.01)
    assertTrue(n1.lastOutput > TRESHOLD + 0.1)
    assertEquals(n1.lastOutput, n2.input,0.01)
  }
  
  @Test
  def shouldRespectTreshold(){
    val n1 = DelayNeuron(TRESHOLD, SLOPE)
    val n2 = DelayNeuron(TRESHOLD, SLOPE)
    n1.connect(n2,1.0)
    n1 += (TRESHOLD - 0.1)

    assertEquals(TRESHOLD - 0.1, n1.input,0.01)
    assertEquals(0.0, n1.getLastTickBuffer,0.01)
    assertEquals(0.0, n1.lastOutput,0.01)
    n1.tick()
    assertEquals(TRESHOLD - 0.1, n1.input,0.01)
    assertEquals(TRESHOLD - 0.1, n1.getLastTickBuffer,0.01)
    assertEquals(0.0, n1.lastOutput,0.01)
    n1.tick()
    assertEquals(TRESHOLD - 0.1, n1.input,0.01)
    assertEquals(TRESHOLD - 0.1, n1.getLastTickBuffer,0.01)
    assertEquals(0.0, n1.lastOutput, 0.01)
    assertEquals(0.0, n2.input,0.01)

  }

}