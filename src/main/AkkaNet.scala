package main

import akka.actor._
import scala.collection.mutable

case object GetNeurons
case class MsgNeurons(neurons: List[AkkaRef])
case class AddNeuron(nref: AkkaRef)

class AkkaNet(val id: String) extends Actor with AbstractNet[AkkaRef] {
  private val neurons = mutable.ListBuffer[AkkaRef]()
  private val ins = mutable.ListBuffer[AkkaRef]()
  private val outs = mutable.ListBuffer[AkkaRef]()
  
  override protected def inputLayer = ins.toSeq
  override protected def outputLayer = outs.toSeq
  override protected def middleLayer = {
    val inputIds = ins.map( _.id ).toSet
    val outputIds = outs.map( _.id ).toSet
    neurons.filterNot( n => inputIds.contains(n.id) || outputIds.contains(n.id))
  }
  
  def receive = {
    case GetId => sender ! Msg(0.0, id)
    case GetNeurons => sender ! MsgNeurons(neurons.toList)
    case AddNeuron(nref) => neurons += nref
  }
}