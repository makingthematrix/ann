package anna.epengine

import anna.Context
import anna.data.NetData
import anna.logger.LOG._
import anna.utils.RandomNumber

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Random

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
      val (newG1, newG2) = cross(iteration, crossedGenomes.size + 1)
      crossedGenomes.append(newG1)
      if(genomesToCross > crossedGenomes.size) crossedGenomes.append(newG2)
    }

    val genomesToClone = results.size - crossedGenomes.size - 1
    val clonedGenomes = if(genomesToClone > 0) { // not so simple, huh
      rest.zipWithIndex.filterNot(tuple => _crossed.contains(tuple._1.id)).take(genomesToClone).map {
        case (ng: NetGenome, i: Int) => clone(ng, iteration, i + 1)
      }
    } else Nil

    debug(this,s"best: ${best.id}, crossedGenomes: ${crossedGenomes.size}, clonedGenomes: ${clonedGenomes.size}")
    debug(this, s" --- new generation done --- ")

    List(best) ++ crossedGenomes ++ clonedGenomes
  }

  private def cross(iteration: Int, index: Int):(NetGenome, NetGenome) = {
    val g1 = drawGenome
    val g2 = drawCrossableGenomeFor(g1) match {
      case Some(genome) => genome
      case None => g1.clone // and then crossing does nothing by definition
    }
    val (ng1, ng2) = cross(g1.clone, g2.clone)
    ng1.netId(generateNewId(iteration, index, GenomeOrigin.CROSSED, g1, g2))
    ng2.netId(generateNewId(iteration, index + 1, GenomeOrigin.CROSSED, g1, g2))
    (ng1, ng2)
  }

  private def clone(genome: NetGenome, iteration: Int, index: Int) = {
    _cloned.add(genome.id)
    genome.netId(generateNewId(iteration, index, GenomeOrigin.CLONED, genome))
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

    val hv = ng2.hushValue
    ng2.hushValue = ng1.hushValue
    ng1.hushValue = hv

    val f = ng2.forgetting
    ng2.forgetting = ng1.forgetting
    ng1.forgetting = f

    val commonSynapsesIds = ng1.synapses.map(s => NetData.removeNetId(s.neuronId)).toSet.intersect(ng2.synapses.map(s => NetData.removeNetId(s.neuronId)).toSet)
    commonSynapsesIds.foreach(id => {
      val sFrom = ng1.getSynapse(id)
      val sTo = ng2.getSynapse(id)
      val w = sTo.weight
      sTo.weight = sFrom.weight
      sFrom.weight = w
    })
  }

  // this method works on ng1 and ng2 - not on their clones - so in fact there is no need for returning them
  // but if I try to make clones of them ( val (nng1, nng2) = (ng1.clone, ng2.clone) ) then
  // SimplerCrosserSuite.shouldSwitchCommonNeuronsSynapse fails. Don't know why.
  override def cross(ng1: NetGenome, ng2: NetGenome):(NetGenome, NetGenome) = {
    debug(this, s"CROSSING: ${ng1.id} with ${ng2.id}")
    _crossed.add(ng1.id)
    _crossed.add(ng2.id)

    val allCommonIds = ng1.fullAccessNeurons.map(n => NetData.removeNetId(n.id)).toSet.intersect(ng2.fullAccessNeurons.map(n => NetData.removeNetId(n.id)).toSet)
    val commonIds = Random.shuffle(allCommonIds).take(math.round(allCommonIds.size * Context().shufflingCoefficient).toInt)

    val commonG1Neurons = ng1.neurons.filter(n => commonIds.contains(NetData.removeNetId(n.id))).toList
    val commonG2Neurons = ng2.neurons.filter(n => commonIds.contains(NetData.removeNetId(n.id))).toList
    val shuffled = shuffleNeurons(commonG1Neurons, commonG2Neurons)
    shuffled.foreach(tuple => switchData(tuple._1, tuple._2))
    (ng1, ng2)
  }
}


object SimplerCrosser {
  def apply(poll: GenomePoll, results: Map[String,Double]) = new SimplerCrosser(poll, results)

  val ID = "SimplerCrosser"

  Crosser.register(ID, new CrosserBuilder {
    override def build(poll: GenomePoll, results: Map[String, Double]) = new SimplerCrosser(poll, results)
  })
}