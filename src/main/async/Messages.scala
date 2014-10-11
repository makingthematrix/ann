package main.async

object Messages {
  // signals
  case class Signal(s: Double)
  case class SignalSeq(input: Seq[Double])
  case object Forgetting
  trait ForgettingTick
  case object ForgetAll extends ForgettingTick
  case class ForgetValue(value: Double) extends ForgettingTick
  case object DontForget extends ForgettingTick

  // commands
  case object Init
  case object HushNow // become silent
  case object WakeUp
  case class Connect(destinationRef: NeuronRef, weight: Double)
  case class Disconnect(destinationId: String)
  case class UpdateSynapse(destinationId: String, synapse: Synapse) // unused
  case class CreateNeuron(id: String, treshold: Double, slope: Double, forgetting: ForgettingTick)
  case class ConnectNeurons(id1: String, id2: String, weight: Double)
  case class SetInputLayer(ids: Seq[String])
  case class SetOutputLayer(ids: Seq[String])
  case object NeuronShutdown
  case object Shutdown // net shutdown
  
  // questions
  case object GetId
  case object GetInput
  case object GetLastOutput
  case class FindSynapse(destinationId: String)
  case object GetSynapses
  case class GetNeuron(id: String)
  case object GetNeurons
  case object GetInputLayer
  case object GetMiddleLayer
  case object GetOutputLayer

  // answers
  abstract class Answer
  case class Success(id: String) extends Answer // successful execution of the command
  case class Failure(error: String) extends Answer // error while executing the command
  case class NeuronShutdownDone(id: String) extends Answer // a special case - successful shutdown of a neuron
  case class NetShutdownDone(id: String) extends Answer // a special case - successful shutdown if the whole net
  case class Msg(d: Double, str: String) extends Answer // general answer to a question about a number or an id
  case class MsgSynapse(synapseOpt: Option[Synapse]) extends Answer // sends back a synapse
  case class MsgSynapses(synapses: List[Synapse]) extends Answer // sends back all synapses of the neuron
  case class MsgNeuron(neuronOpt: Option[NeuronRef]) extends Answer
  case class MsgNeurons(neurons: List[NeuronRef]) extends Answer
	
  // triggers
  case class AddAfterFireTrigger(id: String, f: (Neuron) => Any)

}