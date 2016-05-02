package anna.epengine

import anna.utils.Utils
import anna.utils.Utils.formats
import org.json4s.native.Serialization.{read, writePretty}
import scala.collection.mutable

/**
 * Created by gorywoda on 11/25/15.
 */
case class MutationsProfile(probabilityMap: Map[String, Probability]){
  Utils.assert(probabilityMap.values.map(_.toDouble).sum <= 1.0, "You try to create a mutations profile with the total sum of probabilities > 1.0")

  def size = probabilityMap.size

  private def mutateOnce(gen: NetGenome) = Probability.performRandom(probabilityMap.map{
    case (name, p) => (p, () => MutationsLibrary.get(name)(gen))
  }.toList)

  def mutate(gen: NetGenome, times: Int = 1) = for(i <- 1 to times) mutateOnce(gen)
  def mutate(gen: NetGenome, probability: Probability) = {
    Utils.assert(probability < 1.0, "Don't mutate with probability == 1.0 - it will be never end")
    while(probability.toss) mutateOnce(gen)
  } // mutate until you toss otherwise

  def toJson = writePretty(this)
}

class MutationsProfileBuilder(private val probabilityMap: mutable.Map[String, Probability] = mutable.HashMap[String,Probability]()){
  private def normalize() = {
    val sum = probabilityMap.values.map(_.toDouble).sum
    if(sum > 0.0) probabilityMap.keys.foreach(key =>
      probabilityMap.update(key, probabilityMap(key) / sum)
    )
  }

  private def makeRoom(newProbability: Probability) = {
    val sum = probabilityMap.values.map(_.toDouble).sum
    val t = 1.0 - newProbability
    if(sum > t) {
      val coeff = t / sum
      probabilityMap.keys.foreach(key => probabilityMap.update(key, probabilityMap(key) * coeff))
    }
  }

  def add(name: String, probability: Probability, absolute: Boolean = true):Unit = if(probabilityMap.contains(name)) {
    throw new IllegalArgumentException(s"There is already a mutation with the name $name in the MutationsProfile")
  } else if(!MutationsLibrary.contains(name)){
    throw new IllegalArgumentException(s"There is no mutation with the name $name in the MutationsLibrary")
  } else if(absolute){
    makeRoom(probability)
    probabilityMap += (name -> probability)
  } else {
    probabilityMap += (name -> probability)
    normalize()
  }

  def toMap = probabilityMap.toMap
  def size = probabilityMap.size

  def build = MutationsProfile(probabilityMap.toMap)
}

object MutationsProfile {
  val noMutations = MutationsProfile(Map.empty[String,Probability])

  val simpleMutations = MutationsProfile(
    "addNeuron" -> 0.1,
    "deleteNeuron" -> 0.1,
    "mutateInputTickMultiplier" -> 0.0,
    "addSynapse" -> 0.1,
    "deleteSynapse" -> 0.1,
    "invertSynapse" -> 0.0,
    "setWeightToHush" -> 0.05,
    "setWeightToFull" -> 0.05,
    "mutateWeight" -> 0.2,
    "invertNeuron" -> 0.0,
    "mutateThreshold" -> 0.05,
    "mutateHushValue" -> 0.05,
    "mutateTickTimeMultiplier" -> 0.0,
    "setDontForget" -> 0.05,
    "mutateForgetValue" -> 0.05,
    "setForgetAll" -> 0.05
  )

  def apply(tuples: (String, Double)*):MutationsProfile = {
    val mpb = new MutationsProfileBuilder()
    tuples.foreach{
      case (name, p) => mpb.add(name, p)
    }
    mpb.build
  }

  def fromJson(jsonStr: String) = read[MutationsProfile](jsonStr)
}
