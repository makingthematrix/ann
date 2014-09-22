package main.async

object Messages {
  // signals
  case class Signal(s: Double)
  case class SignalSeq(input: Seq[Double])

  // commands
  case class Init(id: String)
  case object HushNow // become silent
  case class Connect(destinationRef: NeuronRef, weight: Double)
  case class Disconnect(destinationId: String)
  case class UpdateSynapse(destinationId: String, synapse: AkkaSynapse) // unused
  case class CreateNeuron(id: String, treshold: Double, slope: Double, forgetting: Double)
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
  case class MsgSynapse(synapseOpt: Option[AkkaSynapse]) extends Answer // sends back a synapse
  case class MsgSynapses(synapses: List[AkkaSynapse]) extends Answer // sends back all synapses of the neuron
  case class MsgNeuron(neuronOpt: Option[NeuronRef]) extends Answer
  case class MsgNeurons(neurons: List[NeuronRef]) extends Answer
	
  // triggers
  case class AddAfterFireTrigger(id: String, f: (AkkaNeuron) => Any)

}