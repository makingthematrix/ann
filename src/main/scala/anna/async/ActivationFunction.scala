package anna.async

/**
 * Created by gorywoda on 08.05.15.
 */

import anna.utils.Utils

object ActivationFunction {
  val POLYNOMIAL = "polynomial"
  val SIGMOID = "sigmoid"
  val UNUSED = "unused"

  def polynomial(value: Double, slope: Double):Double = Utils.minmax(0.0, slope * (value - 0.5) + 0.5, 1.0)
  def sigmoid(value: Double, slope: Double):Double = Utils.minMaxOpen(value, 0.0, 1.0, 1.0/(1.0+Math.exp(-slope*(value-0.5))) )
  def unused(value: Double, slope: Double): Double = throw new IllegalArgumentException(s"This activation function should not be used!")

  private val map = Map(
    POLYNOMIAL -> polynomial _,
    SIGMOID -> sigmoid _,
    UNUSED -> unused _
  )

  def functionNames = map.keys.toList
  def apply(functionName: String):(Double,Double)=>Double = map(functionName)
}