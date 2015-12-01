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

  def mutate(gen: NetGenome) = Probability.performRandom(probabilityMap.map{
    case (name, p) => (p, () => MutationsLibrary.get(name)(gen))
  }.toList)

  def toJson = writePretty(this)
}

class MutationsProfileBuilder(private val probabilityMap: mutable.Map[String, Probability]){
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
  val noMutationsProfile = MutationsProfile(Map.empty[String,Probability])

  def apply(tuples: (String, Double)*):MutationsProfile
    = MutationsProfile(tuples.map{ case (name, p) => name -> Probability(p) }.toMap)
  def fromJson(jsonStr: String) = read[MutationsProfile](jsonStr)
}
