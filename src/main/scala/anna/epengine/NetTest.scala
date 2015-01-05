package anna.epengine

import anna.async.{NetRef, NetInput}

/**
 * Created by gorywoda on 05.01.15.
 */
case class NetTest(name: String,
                   inputLen: Int,
                   outputIds: List[String],
                   function: (NetInput, NetRef, Double, Double) => Double,
                   success: Double =1.0,
                   failure: Double =0.0) {
  def run(in: NetInput, net: NetRef) = function(in, net, success, failure)
}
