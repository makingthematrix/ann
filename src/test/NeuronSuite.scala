package test

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import org.junit.Assert._
import main.Neuron

class NeuronSuite extends JUnitSuite {
  val SLOPE = 20.0
  val TRESHOLD = 0.5
  
  @Test
  def shouldCreateNeuron(){
    val i = Neuron.getSerialId
    val n = Neuron(TRESHOLD, SLOPE)
    assertEquals(SLOPE,n.slope,0.01)
    assertEquals(TRESHOLD,n.treshold,0.01)
    assertEquals(i,n.id)
  }
  
  @Test
  def shouldConnect(){
    val n1 = Neuron(TRESHOLD, SLOPE)
    val n2 = Neuron(TRESHOLD, SLOPE)
    n1.connect(n2,1.0)
    val s = n1.findSynapse(n2)
    assertFalse(s == None)
  }
  
  @Test
  def shouldGetOutput(){
    val n = Neuron(TRESHOLD, SLOPE)
    n += (TRESHOLD + 0.1)
    n.tick()
    assertTrue(n.lastOutput > 0.0)
  }
  
  @Test
  def shouldSendSignal(){
    val n1 = Neuron(TRESHOLD, SLOPE)
    val n2 = Neuron(TRESHOLD, SLOPE)
    n1.connect(n2,1.0)
    n1 += (TRESHOLD + 0.1)
    n1.tick()
    assertTrue(n2.input > 0.0)
    // should empty the buffer after sending the signal
    assertTrue(n1.input == 0.0)
  }
  
  @Test
  def shouldRespectHardTreshold(){
    val n1 = Neuron(TRESHOLD, SLOPE)
    val n2 = Neuron(TRESHOLD, SLOPE)
    n1.connect(n2,1.0)
    n1 += (TRESHOLD - 0.1)
    assertTrue(n1.input > 0.0)
    n1.tick()
    assertEquals(0.0,n2.input,0.01)
    // should preserve the buffer if sending the signal didn't work
    assertTrue(n1.input > 0.0)
  }

}