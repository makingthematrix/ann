package main

import main.logger.LOG

class Synapse(val source: NeuronLike, val destination: NeuronLike,var weight: Double){
  def send(signal: Double) = {
    val t = signal * weight
    LOG.log( s"sending signal $signal through synapse with weight $weight from ${source.getId} to ${destination.getId} -> $t", source)
    destination += t
  }
}