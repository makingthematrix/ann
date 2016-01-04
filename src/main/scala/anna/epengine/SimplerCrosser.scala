package anna.epengine

import anna.Context
import anna.data.NetData
import anna.logger.LOG._
import anna.utils.RandomNumber

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by gorywoda on 12/27/15.
  */
class SimplerCrosser(override val poll: GenomePoll, override val results:Map[String, Double]) extends Crosser(poll, results){
  // so far, only cloning is implemented. but fear not.
  override def newGeneration(iteration: Int =1): List[NetGenome] = {
    val sortedGenomes = poll.genomesSorted(results)
    val best :: rest = sortedGenomes

    debug(this, " --- new generation --- ")

    val genomesToCross = math.ceil(Context().crossCoefficient * results.size).toInt - 1
    val crossedGenomes = ListBuffer[NetGenome]()
    while(genomesToCross > crossedGenomes.size){
      val (newG1, newG2) = cross
      crossedGenomes.append(newG1)
      if(genomesToCross > crossedGenomes.size) crossedGenomes.append(newG2)
    }

    val genomesToClone = results.size - crossedGenomes.size -1
    val clonedGenomes = if(genomesToClone > 0) {
      rest.zipWithIndex.filterNot(tuple => _crossed.contains(tuple._1.id)).take(genomesToClone).map {
        case (ng: NetGenome, i: Int) => clone(ng, s"iter${iteration}#${i + 1}Cloned")
      }
    } else Nil

    debug(this,s"best: ${best.id}, crossedGenomes: ${crossedGenomes.size}, clonedGenomes: ${clonedGenomes.size}")
    debug(this, s" --- new generation done --- ")

    List(best) ++ crossedGenomes ++ clonedGenomes
  }

  private def cross:(NetGenome, NetGenome) = {
    val ng1 = drawGenome
    val ng2 = drawCrossableGenomeFor(ng1) match {
      case Some(genome) => genome
      case None => ng1 // and then crossing does nothing by definition
    }
    cross(ng1, ng2)
  }

  private def clone(genome: NetGenome, newNetId: String) = {
    debug(this,s"CLONING: genome ${genome.id} as $newNetId")
    _cloned.add(genome.id)
    genome.netId(newNetId)
  }

  private def shuffleNeurons(nlist1: List[NeuronGenome], nlist2: List[NeuronGenome]):List[(NeuronGenome, NeuronGenome)] = {
    val shuffled = ListBuffer[(NeuronGenome, NeuronGenome)]()
    shuffleNeurons(nlist1, nlist2, shuffled)
    shuffled.toList
  }

  @tailrec
  private def shuffleNeurons(nlist1: List[NeuronGenome], nlist2: List[NeuronGenome], result: ListBuffer[(NeuronGenome, NeuronGenome)]):Unit = nlist1 match {
    case Nil =>
    case n1 :: Nil =>
      result.append((n1, nlist2.head))
    case _ =>
      val n1 = RandomNumber(nlist1)
      val n2 = RandomNumber(nlist2)
      result.append((n1, n2))
      shuffleNeurons(nlist1.filterNot(_.id == n1.id), nlist2.filterNot(_.id == n2.id), result)
  }

  private def switchData(ng1: NeuronGenome, ng2: NeuronGenome) = {
    val t = ng2.threshold
    ng2.threshold = ng1.threshold
    ng1.threshold = t

    val s = ng2.slope
    ng2.slope = ng1.slope
    ng1.slope = s

    val hv = ng2.hushValue
    ng2.hushValue = ng1.hushValue
    ng1.hushValue = hv

    val f = ng2.forgetting
    ng2.forgetting = ng1.forgetting
    ng1.forgetting = f

    val ttm = ng2.tickTimeMultiplier
    ng2.tickTimeMultiplier = ng1.tickTimeMultiplier
    ng1.tickTimeMultiplier = ttm

    val commonSynapsesIds = ng1.synapses.map(s => NetData.removeNetId(s.neuronId)).toSet.intersect(ng2.synapses.map(s => NetData.removeNetId(s.neuronId)).toSet)
    commonSynapsesIds.foreach(id => {
      val sFrom = ng1.getSynapse(id)
      val sTo = ng2.getSynapse(id)
      val w = sTo.weight
      sTo.weight = sFrom.weight
      sFrom.weight = w
    })
  }

  override def cross(g1: NetGenome, g2: NetGenome):(NetGenome, NetGenome) = {
    debug(this, s"CROSSING: ${g1.id} with ${g2.id}")
    val commonIds = g1.fullAccessNeurons.map(n => NetData.removeNetId(n.id)).toSet.intersect(g2.fullAccessNeurons.map(n => NetData.removeNetId(n.id)).toSet)
    _crossed.add(g1.id)
    _crossed.add(g2.id)
    val commonG1Neurons = g1.neurons.filter(n => commonIds.contains(NetData.removeNetId(n.id))).toList
    val commonG2Neurons = g2.neurons.filter(n => commonIds.contains(NetData.removeNetId(n.id))).toList
    val shuffled = shuffleNeurons(commonG1Neurons, commonG2Neurons)
    shuffled.foreach(tuple => switchData(tuple._1, tuple._2))
    (g1, g2)
  }
}


object SimplerCrosser {
  def apply(poll: GenomePoll, results: Map[String,Double]) = new SimplerCrosser(poll, results)

  val ID = "SimplerCrosser"

  Crosser.register(ID, new CrosserBuilder {
    override def build(poll: GenomePoll, results: Map[String, Double]) = new SimplerCrosser(poll, results)
  })
}