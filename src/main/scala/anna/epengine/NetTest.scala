package anna.epengine

import anna.async.{NetRef, NetInput}

/**
 * Created by gorywoda on 05.01.15.
 */
case class NetTest(name: String, inputLen: Int, outputIds: List[String], function: (NetInput, NetRef) => Double) {
  def run(in: NetInput, net: NetRef) = function(in, net)
}
