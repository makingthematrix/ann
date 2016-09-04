package anna

import anna.async.NetBuilder
import anna.data.NetData

/**
 * Created by gorywoda on 06.06.15.
 */

class NetDataOps(val data: NetData){
  def <<(input: String) = {
    val netWrapper = NetBuilder().set(data).build()
    val sb = StringBuilder.newBuilder
    if(data.contains("S")) netWrapper.addAfterFire("S")( (_:Double)=>{ sb.append('S'); println('S') } )
    if(data.contains("O")) netWrapper.addAfterFire("O")( (_:Double)=>{ sb.append('O'); println('O') } )
    if(data.contains("dot")) netWrapper.addAfterFire("dot")( (_:Double)=>{ sb.append('.'); println('.') } )
    if(data.contains("line")) netWrapper.addAfterFire("line")( (_:Double)=>{ sb.append('-'); println('-') } )

    netWrapper += input

    netWrapper.tickUntilCalm()
    netWrapper.shutdown()

    sb.toString()
  }
}

object Commands {

  implicit def data2Ops(data: NetData):NetDataOps = new NetDataOps(data)

  def context = Context()

  def print(data: NetData):Unit = println(data.toJson)
  def see(context: Context):Unit = println(context.toJson)

  def build(data: NetData) = NetBuilder().set(data).build()

  def diff(data1: NetData, data2: NetData) ={
    val sb = StringBuilder.newBuilder
    if(data1.id != data2.id) sb.append(s"net id: ${data1.id} -> ${data2.id}\n")
    if(data1.neurons.size != data2.neurons.size) sb.append(s"#neurons: ${data1.neurons.size} -> ${data2.neurons.size}\n")

    val data1NeuronIds = data1.neurons.map(n => NetData.removeNetId(n.id)).toSet
    val data2NeuronIds = data2.neurons.map(n => NetData.removeNetId(n.id)).toSet
    if(data1NeuronIds != data2NeuronIds){
      sb.append(s"neurons: ${data1NeuronIds.toList.sorted} -> ${data2NeuronIds.toList.sorted}\n")
      (data1NeuronIds -- data2NeuronIds).foreach( nid => sb.append(s"$nid deleted\n"))
      (data2NeuronIds -- data1NeuronIds).foreach( nid => {
        val n = data2.neuron(NetData.neuronId(data2.id, nid))
        sb.append(s"$nid added: $n\n")
      })
    }

    data1NeuronIds.intersect(data2NeuronIds).foreach(nid => {
      val neuronId1 = NetData.neuronId(data1.id, nid)
      // constant neurons have no netId before neuronId so but we don't know which are they so we have to check both possibilities
      val n1 = if(data1.contains(neuronId1)) data1.neuron(neuronId1) else data1.neuron(nid)
      val neuronId2 = NetData.neuronId(data2.id, nid)
      val n2 = if(data2.contains(neuronId2)) data2.neuron(neuronId2) else data2.neuron(nid)
      if(n1 != n2){
        if(n1.threshold != n2.threshold) sb.append(s"$nid threshold: ${n1.threshold} -> ${n2.threshold}\n")
        if(n1.hushValue != n2.hushValue) sb.append(s"$nid hushValue: ${n1.hushValue} -> ${n2.hushValue}\n")

        val n1SynapsesMap = n1.synapses.map(s => (s.neuronId -> s.weight)).toMap
        val n2SynapsesMap = n2.synapses.map(s => (s.neuronId -> s.weight)).toMap

        if(n1SynapsesMap.keySet != n2SynapsesMap.keySet){
          sb.append(s"$nid synapses: ${n1SynapsesMap.keys.toList.sorted} -> ${n2SynapsesMap.keys.toList.sorted}\n")
          (n1SynapsesMap.keySet -- n2SynapsesMap.keySet).foreach(sid => sb.append(s"${nid}->${sid} deleted\n"))
          (n2SynapsesMap.keySet -- n1SynapsesMap.keySet).foreach(sid => {
            sb.append(s"${nid}->${sid} added ${n2SynapsesMap(sid)}\n")
          })
        }

        n1SynapsesMap.keySet.intersect(n2SynapsesMap.keySet).foreach(sid => {
          val s1 = n1SynapsesMap(sid)
          val s2 = n2SynapsesMap(sid)
          if(s1 != s2) sb.append(s"${nid}->${sid} weight: ${n1SynapsesMap(sid)} -> ${n2SynapsesMap(sid)}\n")
        })
      }
    })

    sb.toString
  }

}
