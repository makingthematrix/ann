package anna.utils

import scala.concurrent.Await
import anna.async.Context.timeout
import akka.actor.ActorRef
import akka.pattern.ask
import anna.async.NetRef
import anna.async.NeuronRef

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
    // = 2/(1+EXP(-C*x))-1 ; mapowanie S -1->-1,0->0,1->1, gdzie C to stromość
    // = 1/(1+EXP(-C*(x-0.5))) ; mapowanie S 0->0,0.5->0.5,1->1, gdzie C to stromość
  
}

