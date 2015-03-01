package anna.epengine

import anna.async.{NetRef, NetWrapper}

/**
 * Created by gorywoda on 05.01.15.
 */
case class NetTest(name: String,
                   inputLen: Int,
                   outputIds: List[String],
                   function: (NetWrapper, Double, Double) => Double,
                   success: Double =1.0,
                   failure: Double =0.0) {
  def run(wrapper: NetWrapper) = function(wrapper, success, failure)
}
