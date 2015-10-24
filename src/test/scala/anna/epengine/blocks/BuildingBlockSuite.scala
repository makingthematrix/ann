package anna.epengine.blocks

import anna.async.MySuite
import anna.data.{Hush, SynapseWeight, SynapseTrait, NetData}
import anna.utils.Utils
import org.junit.Test
import org.junit.Assert._

/**
 * Created by gorywoda on 10/23/15.
 */
class BuildingBlockSuite  extends MySuite {
  // BuildingBlock: składa się z neuronów i synapse między nimi, oraz z "fantomowych" neuronów wejścia o nazwach ("in1","in2",,..,"inN")
  // oraz "fantomowych" neuronów wyjścia o nazwach ("out1","out2",...,"outM")
  // w momencie konwersji bloku na NetData te fantomowe neurony i połączenia z nimi znikają
  // natomiast gdy łączymy dwa bloki, trzeba określić jak "out" jednego łączy się z "in" drugiego.
  @Test def shouldBuildOneNeuronNetFromBlock(): Unit = {
    val block:BuildingBlock = BuildingBlock.createBlock("oneneuronbridge")
    // blok powinien zawierać 3 neurony: wejściowy, domyślny i fantomowy wyjściowy
    assertEquals(3, block.neurons.size)
    assertEquals(1, block.ins.size)
    assertEquals(1, block.phantomOuts.size)

    assertTrue(block.contains("in1"))
    assertTrue(block.contains("default"))
    assertTrue(block.contains("out1"))

    // domyślna nazwa neuronu w template powinna brzmieć "default" - zmieniamy ją
    block.changeNeuronId("default","n1")

    // blok powinien zawierać 2 synapsy "in1"->"n1" (po zmianie nazwy), oraz "n1"->"out1"
    assertEquals(2, block.synapses.size)
    assertTrue(block.contains("in1","n1"))
    assertTrue(block.contains("n1","out1"))

    // możemy zmieniać ich wagi
    val weight05: SynapseTrait = SynapseWeight(0.5)
    block.setWeight("in1","n1", weight05)
    assertEquals(weight05, block.weight("in1","n1"))
    block.setWeight("in1","n1", Hush())
    assertEquals(Hush(), block.weight("in1","n1"))

    // ale po utworzeniu netData mamy już tylko dwa neurony i jedną synapsę in1->n1
    // domyślnie, przy przerabianiu bloku na sieć, fantomowe neurony wyjściowe są usuwane.
    val netData1:NetData = block.netData
    assertTrue(netData1.contains("in1"))
    assertTrue(netData1.contains("n1"))
    assertEquals(2, netData1.neurons.size)
    assertEquals(1, netData1.synapses.size)
    assertTrue(netData1.synapses.contains(Utils.synapseId("in1","n1")))
    assertEquals(1, netData1.inputs.size)
    assertTrue(netData1.inputs.contains("n1"))

    // można jednak zaznaczyć, że nie chcemy ich usuwać

    assertTrue(block.removePhantomOuts)
    block.removePhantomOuts = false

    val netData2:NetData = block.netData
    assertTrue(netData2.contains("n1"))
    assertTrue(netData2.contains("in1"))
    assertTrue(netData2.contains("out1"))
    assertEquals(2, netData2.synapses.size)
    assertTrue(netData2.synapses.contains(Utils.synapseId("in1","n1")))
    assertTrue(netData2.synapses.contains(Utils.synapseId("n1","out1")))
  }
}
