package anna.data

import anna.Context
import anna.async._
import anna.utils.Utils.{formats, classTagToManifest}
import org.json4s.native.Serialization.{read, writePretty}
import scala.reflect.ClassTag

final case class NeuronData(id: String,
                      threshold: Double,
                      silenceIterations: Int,
                      synapses: List[SynapseData],
                      neuronType: NeuronType){
  def withId(id: String): NeuronData = copy(id = id)
  def withThreshold(threshold: Double): NeuronData = copy(threshold = threshold)
  def withSilenceIterations(silenceIterations: Int): NeuronData = copy(silenceIterations = silenceIterations)
  def withSynapses(synapses: List[SynapseData]): NeuronData = copy(synapses = synapses)
  def withoutSynapses: NeuronData = withSynapses(Nil)
  def withNeuronType(neuronType: NeuronType): NeuronData = copy(neuronType = neuronType)

  def toJson: String = writePretty(this)

  def isConnectedTo(id: String): Boolean = synapses.exists(_.neuronId == id)
}

object NeuronData {
  import NeuronType.*
  
  def apply(id: String, threshold: Double, silenceIterations: Int, synapses: List[SynapseData]): NeuronData = 
    apply(id, threshold, silenceIterations, synapses, NeuronTypeStandard)

  def apply(id: String, silenceIterations: Int): NeuronData = apply(id, 0.0, silenceIterations, Nil, NeuronTypeDummy)
  
  def apply(id: String): NeuronData = apply(id, 0.0, Context().silenceIterations, Nil, NeuronTypeSilencing)

  def fromJson(jsonStr: String)(using ct: ClassTag[NeuronData]): NeuronData = read[NeuronData](jsonStr)
}