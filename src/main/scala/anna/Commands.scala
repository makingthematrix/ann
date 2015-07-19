package anna

import anna.Context._
import anna.async.NetBuilder
import anna.data.NetData
import anna.epengine._
import anna.logger.LOG
import anna.utils.Utils
import anna.logger.LOG._
import anna.async.NetBuilderOps._
import anna.utils.DoubleRange._

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

  def listEvolutions = {
    val dirs = Utils.listDirs(Context().evolutionDir)
    println(s"the evolutions directory is ${Context().evolutionDir}")
    println(s"evolutions found: ${dirs.size}")
    dirs.foreach( println )
  }

  def listSets = {
    val sets = Utils.listFiles(Context().exercisesSetDir)
    println(s"the exercises sets directory is ${Context().exercisesSetDir}")
    println(s"sets found: ${sets.size}")
    sets.map(s => s.substring(0,s.lastIndexOf(".json"))).foreach( println )
  }

  private var exercisesSetOpt:Option[ExercisesSet] = None

  def set(name: String):Unit = {
    exercisesSetOpt = Some(ExercisesSet.load(name))
  }

  def set:String = exercisesSetOpt.map(_.name).getOrElse("The exercises set is not set")

  private var engineOpt:Option[Engine] = None

  def engine = engineOpt.getOrElse(throw new IllegalArgumentException("No engine set"))
  def poll = engine.poll
  def coach = engine.coach

  lazy val sosTemplate = {
    val data = NetBuilder().addInput("in").chain("mi11",1.0,0.5).chain("mi12",1.0,0.5).chain("dot",1.0,0.5).chain("S",1.0,0.5)
      .use("in").chain("mi21",1.0,0.5).chain("mi22",1.0,0.5).chain("line",1.0,0.5).chain("O",1.0,0.5)
      .use("mi12").hush("mi21")
      .use("mi21").hush("mi11")
      .use("dot").hush("line").hush("O")
      .use("line").hush("dot").hush("S")
      .data
    val genome = NetGenome(data, accessMapOpt.get)
    genome.netId("dotline").data
  }

  lazy val dotLineData = {
    val data = NetBuilder().addInput("in").chain("mi11",1.0,0.5).chain("mi12",1.0,0.5).chain("dot",1.0,0.5)
      .use("in").chain("mi21",1.0,0.5).chain("mi22",1.0,0.5).chain("line",1.0,0.5)
      .use("mi12").hush("mi21")
      .use("mi21").hush("mi11")
      .use("dot").hush("line")
      .use("line").hush("dot")
      .data
    val genome = NetGenome(data, accessMapOpt.get)
    genome.netId("dotline").data
  }

  // hush instead of standard synapses - for debugging purposes
  lazy val dotLineHushData = {
    val data = NetBuilder().addInput("in").chainHush("mi11",0.5).chainHush("mi12",0.5).chainHush("dot",0.5)
      .use("in").chainHush("mi21",0.5).chainHush("mi22",0.5).chainHush("line",0.5)
      .use("mi12").hush("mi21")
      .use("mi21").hush("mi11")
      .use("dot").hush("line")
      .use("line").hush("dot")
      .data
    val genome = NetGenome(data, accessMapOpt.get)
    genome.netId("dotlinehush").data
  }

  val simplestData = NetBuilder().addInput("in").chain("dot",1.0,0.5).use("in").chain("line",1.0,0.5).setName("simplest").data
  val zeroData = NetBuilder().addInput("in").chain("dot",0.0,0.0).use("in").chain("line",0.0,0.0).setName("zero").data

  val sosNetData = NetBuilder().SOSNetData()

  private var accessMapOpt: Option[Map[String, MutationAccess]] = None

  def accessMap:String = accessMapOpt.map(_.toString).getOrElse("Access map not set")

  def accessMap(map: Map[String, MutationAccess]):Unit = {
    accessMapOpt = Some(map)
  }

  def setDotLineAccessMap() = accessMap(Map("in" -> MutationAccessDontMutate(), "dot" -> MutationAccessDontDelete(), "line" -> MutationAccessDontDelete()))

  private var inputIdsOpt:Option[List[String]] = None

  def inputIds(ids:String*): Unit ={
    inputIdsOpt = Some(ids.toList)
  }

  def inputIds:String = inputIdsOpt.map(_.mkString(", ")).getOrElse("Input ids not set")

  private var outputIdsOpt:Option[List[String]] = None

  def outputIds(ids:String*): Unit ={
    outputIdsOpt = Some(ids.toList)
  }

  def outputIds:String = outputIdsOpt.map(_.mkString(", ")).getOrElse("Output ids not set")

  def setDotLineConfig() = {
    setDotLineAccessMap()
    inputIds("in")
    outputIds("dot", "line")
    set("dotlineset")
    println("dot-line config set")
  }

  def create(name: String, template: NetData) = {
    val inputIds:List[String] = inputIdsOpt.getOrElse(throw new IllegalArgumentException("No input ids set"))
    val outputIds:List[String] = outputIdsOpt.getOrElse(throw new IllegalArgumentException("No output ids set"))
    val exercisesSet = exercisesSetOpt.getOrElse(throw new IllegalArgumentException("No exercises set... set"))
    engineOpt = Some(Engine(name, inputIds, outputIds, template, exercisesSet))
    println("done")
  }

  def open(name: String) = {
    engineOpt = Some(Engine(name))
    println("done")
  }

  def run(iterations: Int =1) = engineOpt match {
    case Some(engine) => engine.run(iterations)
    case None => exception(this, "Unable to run as no engine is ready")
  }

  def results = engine.results
  def avg = engine.avg
  def median = engine.median
  def quintiles = engine.quintiles
  def quintile(n: Int) = engine.quintile(n)
  def runWithStats(iterations: Int =20) = engine.runWithStats(iterations)

  def best = engineOpt match {
    case Some(engine) => engine.best.data
    case None => throw new IllegalArgumentException("Unable to get the best genome as no engine is ready")
  }

  def print(data: NetData):Unit = println(data.toJson)
  def see(context: Context):Unit = println(context.toJson)

  def build(data: NetData) = NetBuilder().set(data).build()

  def test(data: NetData, exerciseName: String):Unit = {
    val exercise = ExercisesLibrary(exerciseName)
    val coach = Coach(List(exercise))
    val result = coach.test(data)
    println("--- result: " + result)
  }

  def test(data: NetData):Unit = {
    val result = exercisesSetOpt match {
      case Some(exercisesSet) => Coach(exercisesSet).test(data)
      case None => throw new IllegalArgumentException("No exercises set... set")
    }
    println("--- result: " + result)
  }

  def mutate(data: NetData) = {
    val accessMap = accessMapOpt.getOrElse(throw new IllegalArgumentException("No access map set"))
    val genome = NetGenome(data, accessMap)
    genome.mutate()
    genome.data
  }

  def cross(data1: NetData, data2: NetData):(NetData,NetData) = {
    val accessMap = accessMapOpt.getOrElse(throw new IllegalArgumentException("No access map set"))
    val genome1 = NetGenome(data1, accessMap)
    val genome2 = NetGenome(data2, accessMap)
    if(genome1.crossable(genome2)) {
      val (newGenome1, newGenome2) = genome1.cross(genome2)
      (newGenome1.data, newGenome2.data)
    } else throw new IllegalArgumentException(s"${data1.id} not crossable with ${data2.id}")
  }

  def diff(data1: NetData, data2: NetData) ={
    val sb = StringBuilder.newBuilder
    if(data1.id != data2.id) sb.append(s"net id: ${data1.id} -> ${data2.id}\n")
    if(data1.inputTickMultiplier != data2.inputTickMultiplier) sb.append(s"input tick multiplier: ${data1.inputTickMultiplier} -> ${data2.inputTickMultiplier}")
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
        if(n1.slope != n2.slope) sb.append(s"$nid slope: ${n1.slope} -> ${n2.slope}\n")
        if(n1.hushValue != n2.hushValue) sb.append(s"$nid hushValue: ${n1.hushValue} -> ${n2.hushValue}\n")
        if(n1.forgetting != n2.forgetting) sb.append(s"$nid forgetting: ${n1.forgetting} -> ${n2.forgetting}\n")
        if(n1.tickTimeMultiplier != n2.tickTimeMultiplier) sb.append(s"$nid tickTimeMultiplier: ${n1.tickTimeMultiplier} -> ${n2.tickTimeMultiplier}\n")

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

  def contextMatrix(iterations: Int = 2, name: String, template: NetData): Unit = {
    val cm = ContextMatrix(List(
     // ContextDoubleRange(_mutationprobability, 0.2 <=> 0.8, 3),
     // ContextDoubleRange(_crosscoefficient, 0.2 <=> 0.8, 3),
     // ContextDoubleRange(_hushprobability, 0.2 <=> 0.8, 3),
     // ContextDoubleRange(_fullweightprobability, 0.2 <=> 0.8, 3),
      ContextDoubleRange(_addneuronprobability, 0.2 <=> 0.8, 3),
      ContextDoubleRange(_addsynapseprobability, 0.2 <=> 0.8, 3)
     // ContextDoubleRange(_mutateneuronprobability, 0.2 <=> 0.8, 3)
    ))

    /* write to csv */
    val sb = StringBuilder.newBuilder

    //sb.append(s"${_mutationprobability},")
    //sb.append(s"${_crosscoefficient},")
   // sb.append(s"${_hushprobability},")
    //sb.append(s"${_fullweightprobability},")
    sb.append(s"${_addneuronprobability},")
    sb.append(s"${_addsynapseprobability},")
    //sb.append(s"${_mutateneuronprobability},")
    sb.append("iteration,")
    sb.append("bestId,")
    sb.append("bestResult,")
    sb.append("avg,")
    sb.append("size,")
    sb.append("median,")
    sb.append("quintiles(0),")
    sb.append("quintiles(1),")
    sb.append("quintiles(2),")
    sb.append("quintiles(3),")
    sb.append("quintiles(4)\n")

    cm.unfold.map(contextVector => {
      Context.set(contextVector)
      create(s"name${System.currentTimeMillis()}", template)
      val stats = engine.runWithStats(iterations)

      val map = contextVector
      val lastIter = stats.last

      //sb.append(s"${map(_mutationprobability)},")
      //sb.append(s"${map(_crosscoefficient)},")
      //sb.append(s"${map(_hushprobability)},")
      //sb.append(s"${map(_fullweightprobability)},")
      sb.append(s"${map(_addneuronprobability)},")
      sb.append(s"${map(_addsynapseprobability)},")
      //sb.append(s"${map(_mutateneuronprobability}),")
      sb.append(s"${lastIter.iteration},")
      sb.append(s"${lastIter.bestId},")
      sb.append(s"${lastIter.bestResult},")
      sb.append(s"${lastIter.avg},")
      sb.append(s"${lastIter.size},")
      sb.append(s"${lastIter.median},")
      sb.append(s"${lastIter.quintiles(0)},")
      sb.append(s"${lastIter.quintiles(1)},")
      sb.append(s"${lastIter.quintiles(2)},")
      sb.append(s"${lastIter.quintiles(3)},")
      sb.append(s"${lastIter.quintiles(4)}\n")
      Utils.save(s"contextMatrixTest-$name", sb.toString() )
      sb.clear()
    })
  }
}
