package main

object Utils {
  def fail(str: String):IllegalArgumentException = throw new IllegalArgumentException(str)
  def assert(condition: => Boolean, str: String) = if(!condition) fail(str)
  
  def minmax(min: Double, v: Double, max: Double) = Math.max(min,Math.min(max,v))
  
  /* @todo: This is weird, but it seems there has to be a sharp boundary ( <= and >= instead of < and >), 
  *  because it somehow rounds values >0.92 to 1.0 which is needed by the dotLineNet.
  *  Otherwise the signals are not strong enough.
  */
  def minmax(x: Double, min: Double, max: Double, d: => Double) = x match {
    case _ if x <= min => min
    case _ if x >= max => max
    case _ => d
  }  

}

