package anna.async

import anna.Context
import anna.data.NetData
import anna.logger.LOG
import org.junit.Assert._
import org.hamcrest.CoreMatchers._
import org.junit.{After, Before}
import org.scalatest.junit.JUnitSuite

class MySuite extends JUnitSuite {
  private var _builder: NetBuilder = _
  def builder = _builder
  private var _netWrapper: NetWrapper = null
  def netWrapper = _netWrapper
  private var _oldContext:Context = _


  @Before def before(){
    LOG.addLogToStdout()
    _builder = NetBuilder()
    _oldContext = Context()
  }
  
  @After def after(){
    if(_netWrapper != null) _netWrapper.shutdown()
    _builder = null
    _netWrapper = null
    Context.set(_oldContext)
    shutdown()
    LOG.date()
  }

  protected def shutdown(): Unit ={
    if(_netWrapper != null) _netWrapper.shutdown()
    _netWrapper = null
  }
  
  protected def build() = {
    _netWrapper = _builder.build("net")
  }

  protected def build(data: NetData) = {
    _builder.set(data)
    _netWrapper = _builder.build("net")
  }

  protected def setNetWrapper(netWrapper: NetWrapper): Unit ={
    _netWrapper = netWrapper
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

  protected def assertNotEquals[T](a: T, b: T) = assertThat(a, is(not(b)))
}