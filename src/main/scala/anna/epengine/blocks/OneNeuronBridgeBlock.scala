package anna.epengine.blocks

import anna.async.NetBuilder
import anna.async.NetBuilderOps._

/**
 * Created by gorywoda on 10/23/15.
 */

object OneNeuronBridgeBlock {
  def apply() = NetBuilder().addInput("in1")
                            .chain("default",1.0,0.0)
                            .chain("out1",1.0,0.0)
                            .setName("oneneuronbridge")
                            .buildingBlock(Set("out1"))
}