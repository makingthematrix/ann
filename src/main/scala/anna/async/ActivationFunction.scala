package anna.async

/**
 * Created by gorywoda on 08.05.15.
 */

import anna.utils.Utils

object ActivationFunction {
  val SIGMOID = "sigmoid"
  val STEP = "step"
  val THRESHOLD_LOGIC = "thresholdLogic"
  val UNUSED = "unused"

  // 'step' works with the neuron's threshold to create a step function with the step being exactly at the threshold
  def step(value: Double, slope: Double):Double = 1.0
  // 'thresholdLogic' uses a lineral function y=ax with 'a' being the slope
  def thresholdLogic(value: Double, slope: Double):Double = Utils.minmax(0.0, slope * (value - 0.5) + 0.5, 1.0)
  // 'sigmoid' is a "traditional" 1/(1+e^x) where x is the slope
  def sigmoid(value: Double, slope: Double):Double = Utils.minMaxOpen(value, 0.0, 1.0, 1.0/(1.0+Math.exp(-slope*(value-0.5))) )
  // 'unused' is for Hush and Dummy neurons which don't use the activation function at all
  def unused(value: Double, slope: Double): Double = throw new IllegalArgumentException(s"This activation function should not be used!")

  private val map = Map(
    STEP -> step _,
    SIGMOID -> sigmoid _,
    THRESHOLD_LOGIC -> thresholdLogic _,
    UNUSED -> unused _
  )

  def functionNames = map.keys.toList
  def apply(functionName: String):(Double,Double)=>Double = map(functionName)
}