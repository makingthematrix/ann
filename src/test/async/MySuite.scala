package test.async

import org.scalatest.junit.JUnitSuite
import main.async.NetBuilder
import main.async.NetInput
import main.async.NetRef
import org.junit.Before
import org.junit.After
import main.async.logger.LOG
import org.junit.Assert._
import scala.concurrent.Promise
import main.async.Neuron
import scala.concurrent.Await
import main.async.logger.LOG.debug
import scala.concurrent.duration._

class MySuite extends JUnitSuite {
  private var _builder: NetBuilder = _
  def builder = _builder
  private var _in: NetInput = _
  def in = _in
  private var _net: NetRef = _
  def net = _net
  
  @Before def before(){
    LOG.addLogToStdout()
    _builder = NetBuilder()
  }
  
  @After def after(){
    if(_net != null) _net.shutdown()
    _builder = null
    _in = null
    _net = null
    LOG.date()
  }
  
  protected def build() = {
    val triple = _builder.build("in1","out1")
    _in = triple._1
    _net = triple._2
  }
  
  protected def assertEqualsWithTolerance(expected: Seq[Long], received: Seq[Long], tolerance: Long) = {
    assertEquals(expected.size, received.size)
    expected.zip(received).foreach( tuple => if(math.abs(tuple._1 - tuple._2) > tolerance) fail(s"""
        Expected: ${expected}\n
        Received: ${received}\n
        The values ${tuple._1} and ${tuple._2} vary by ${math.abs(tuple._1 - tuple._2)} which is more than the asserted tolerance $tolerance.
    """))
  }
  
  protected def produceSeq(size: Int, ini: Long, off: Long):Seq[Long] = size match {
    case 1 => Seq(ini)
    case x if x > 1 => produceSeq(x-1, ini, off) ++ Seq(ini+(x-1)*off)
  }
  
  protected def init() = {
    LOG.timer()
  }
}