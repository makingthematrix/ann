package anna.async

import anna.async.NeuronTriggers.Trigger
import anna.data.{NeuronData, SynapseTrait}

object Messages {
  sealed trait Message

  // signals
  sealed trait Sig extends Message
  final case class Signal(s: Double, senderId: String) extends Sig
  final case class SignalList(inputs: List[Double]) extends Sig

  // commands
  sealed trait Command extends Message
  case object SilenceRequest extends Command// become silent
  case object WakeRequest extends Command// force wake up
  final case class Connect(destinationRef: NeuronRef, weight: SynapseTrait) extends Command
  final case class Disconnect(destinationId: String) extends Command
  final case class CreateNeuron(data: NeuronData) extends Command
  final case class SetInputs(ids: Seq[String]) extends Command
  //case object NeuronShutdown extends Command
  case object Shutdown extends Command // net shutdown
  case object Reset extends Command
  case object RemoveAllTriggers extends Command
  final case class SetSynapses(synapses: Seq[Synapse]) extends Command

  // questions
  sealed trait Question extends Message
  case object GetId extends Question
  case object GetInput extends Question
  case object GetLastOutput extends Question
  final case class FindSynapse(destinationId: String) extends Question
  case object GetSynapses extends Question
  final case class GetNeuron(id: String) extends Question
  case object GetNeurons extends Question
  case object GetInputs extends Question
  case object GetData extends Question

  // answers
  sealed trait Answer extends Message
  final case class Success(id: String) extends Answer // successful execution of the command
  final case class Failure(error: String) extends Answer // error while executing the command
  final case class NeuronShutdownDone(id: String) extends Answer // a special case - successful shutdown of a neuron
  final case class NetShutdownDone(id: String) extends Answer // a special case - successful shutdown if the whole net
  final case class Msg(d: Double, str: String) extends Answer // general answer to a question about a number or an id
  final case class MsgSynapse(synapseOpt: Option[Synapse]) extends Answer // sends back a synapse
  final case class MsgSynapses(synapses: Seq[Synapse]) extends Answer // sends back all synapses of the neuron
  final case class MsgNeuron(neuronOpt: Option[NeuronRef]) extends Answer
  final case class MsgNeurons(neurons: Seq[NeuronRef]) extends Answer
  final case class NeuronInfo(id: String,
                        netId: String,
                        threshold: Double,
                        silenceIterations: Int,
                        synapses: List[SynapseInfo],
                        buffer: Double)
  final case class SynapseInfo(neuronId: String, weight: SynapseTrait) extends Answer


  // triggers
  sealed trait Trig extends Message
  final case class AddAfterFireTrigger(id: String, f: Trigger) extends Trig
  final case class RemoveAfterFireTrigger(id: String) extends Trig
  final case class AddSilenceRequestedTrigger(id: String, f: Trigger) extends Trig
  final case class RemoveSilenceRequestedTrigger(id: String) extends Trig
  final case class AddSignalIgnoredTrigger(id: String, f: Trigger) extends Trig
  final case class RemoveSignalIgnoredTrigger(id: String) extends Trig
}
