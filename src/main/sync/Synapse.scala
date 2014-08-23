package main.sync

import main.logger.LOG

class Synapse(val source: Neuron, val destination: Neuron,var weight: Double){
  def send(signal: Double) = {
    val t = signal * weight
    LOG.log( s"sending signal $signal through synapse with weight $weight from ${source.id} to ${destination.id} -> $t", source)
    destination += t
  }
}