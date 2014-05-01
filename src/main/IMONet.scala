package main

import scala.collection.Seq

class IMONet(val ins: Seq[DummyNeuron],val mids: Seq[Neuron],val outs: Seq[DummyNeuron]) extends AbstractNet {
  override protected def inputLayer = ins
  override protected def middleLayer = mids
  override protected def outputLayer = outs
  
  def weight(id1: String, id2: String):Option[Double] = {
    val (n1,n2) = find(id1,id2)
    n1.findSynapse(n2) match {
      case Some(s) => Some(s.weight)
      case None => None
    }
  }
  
  def connect(id1: String, id2: String, weight: Double, updateIfExist:Boolean = true){
    val (n1,n2) = find(id1,id2)
    n1.findSynapse(n2) match {
      case Some(s) if updateIfExist => s.weight = weight
      case Some(s) if !updateIfExist => 
      case None => n1.connect(n2, weight)
    }
  }
  
  def disconnect(id1: String, id2: String, throwIfDisconnectedAlready:Boolean = false){
    val (n1,n2) = find(id1,id2)
    n1.findSynapse(n2) match {
      case Some(s) => n1.disconnect(n2) 
      case None if throwIfDisconnectedAlready => throw new IllegalArgumentException(s"Neurons $id1 and $id2 are already disconnected")
      case None if !throwIfDisconnectedAlready =>
    }
  }
}

object IMONet {
  private var defSlope = 20.0
  private var defTreshold = 0.5
  private var defWeight = 1.0
  
  private var serialId = 1L;
  
  def apply(inSize: Int, midSize: Int, outSize: Int) = {
    val ins = for(i <- 1 to inSize) yield DummyNeuron()
    val mids = for(i <- 1 to midSize) yield Neuron(defTreshold, defSlope)
    val outs = for(i <- 1 to outSize) yield DummyNeuron()
    ins.zip(mids).foreach( tuple => tuple._1.connect(tuple._2, defWeight) )
    mids.zip(outs).foreach( tuple => tuple._1.connect(tuple._2, defWeight) )
    new IMONet(ins, mids, outs)
  }
}