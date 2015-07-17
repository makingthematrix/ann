package anna.epengine

/**
 * Created by gorywoda on 17.07.15.
 */
case class EvolutionStats(iteration: Int,
                          bestId: String,
                          bestResult: Double,
                          avg: Double,
                          size: Int,
                          median: Double,
                          quintiles: Map[Int, Double])
