package anna.epengine

import anna.utils.Utils

import scala.collection.mutable

/**
 * Created by gorywoda on 11/25/15.
 */
class MutationsProfile(private val probabilityMap: mutable.Map[String, Probability]){
  private def normalize() = {
    val sum = probabilityMap.values.map(_.toDouble).sum
    // if this method is called, we assume there is at least one non-zero probability in the map
    probabilityMap.keys.foreach(key => {
      probabilityMap.update(key, probabilityMap(key) / sum)
    })
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

  def mutate(net: NetGenome) = Probability.performRandom(probabilityMap.map{ case (name, p) => (p, () => MutationsLibrary.get(name)(net))}.toList)
}

object MutationsProfile {
  def apply(map: Map[String, Double]) = {
    Utils.assert(map.values.sum <= 1.0, "You try to create a mutations profile with the total sum of probabilities > 1.0")
    val probabilityMap = mutable.Map[String, Probability]()
    map.foreach{ case (name, d) => probabilityMap += (name -> Probability(d))}
    new MutationsProfile(probabilityMap)
  }
}
