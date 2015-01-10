package anna.utils

import java.text.{NumberFormat, ParsePosition}
import java.util.Locale

import akka.actor.ActorRef
import akka.pattern.ask
import anna.Context
import anna.Context.timeout
import anna.async.{NetRef, NeuronRef}
import anna.logger.LOG._

import scala.concurrent.Await

object Utils {
  def fail(str: String):IllegalArgumentException = throw new IllegalArgumentException(str)
  def assert(condition: => Boolean, str: String) = if(!condition) fail(str)
  
  def minmax(min: Double, v: Double, max: Double) = Math.max(min,Math.min(max,v))
  
  /* @todo: This is weird, but it seems there has to be a sharp boundary ( <= and >= instead of < and >), 
  *  because it somehow rounds values >0.92 to 1.0 which is needed by the dotLineNet.
  *  Otherwise the signals are not strong enough.
  */
  def minMaxClosed(x: Double, min: Double, max: Double, d: => Double) = x match {
    case _ if x <= min => min
    case _ if x >= max => max
    case _ => d
  }
  
  def minMaxOpen(x: Double, min: Double, max: Double, d: => Double) = x match {
    case _ if x < min => min
    case _ if x > max => max
    case _ => d
  }

  def await[T](ref: ActorRef, msg: Any): T = Await.result(ref ? msg, timeout.duration).asInstanceOf[T]
  def await[T](net: NetRef, msg: Any): T = await[T](net.ref, msg)
  def await[T](neuron: NeuronRef, msg: Any): T = await[T](neuron.ref, msg)
  
  def f(value: Double, slope: Double) = minMaxOpen(value, 0.0, 1.0, 1.0/(1.0+Math.exp(-slope*(value-0.5))) )
  def s(value: Double, slope: Double) = minmax(0.0, slope * (value - 0.5) + 0.5, 1.0)

  def parseDouble(s: String) = {
    debug(this, s"parsing $s")
    val pp = new ParsePosition(0)
    val nf = NumberFormat.getInstance(Locale.ENGLISH)
    val d = nf.parse(s, pp)
    if (pp.getErrorIndex == -1) Some(d.doubleValue) else None
  }

}

