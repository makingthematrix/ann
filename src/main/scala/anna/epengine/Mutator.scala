package anna.epengine

import anna.Context
import anna.logger.LOG._
import anna.utils.RandomNumber

/**
  * Created by gorywoda on 12/26/15.
  */
class Mutator(val mutationsProfile: MutationsProfile) {
  def mutate(genomes: List[NetGenome]) = {
    debug(this, s" --- mutating --- ")
    genomes.foreach( g => if(Probability(Context().mutationProbability).toss) {
      mutationsProfile.mutate(g, RandomNumber(Context().mutationsPerGenome))
      g.data.validate()
    })
    debug(this, s" --- done mutating --- ")
    genomes
  }
}

object Mutator {
  def apply(mutationsProfile: MutationsProfile) = new Mutator(mutationsProfile)
}