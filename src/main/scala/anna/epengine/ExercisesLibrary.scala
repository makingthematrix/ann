package anna.epengine

import anna.Context
import anna.async.NetWrapper

import scala.util.Random
import scala.collection.mutable

/**
 * Created by gorywoda on 11.03.15.
 */

abstract class Exercise(val name: String, val inputLen: Int, val outputIds: List[String]) {
  def run(wrapper: NetWrapper): Double
  def successIf(flag: Boolean) = if(flag) 1.0 else 0.0

  ExercisesLibrary.add(name, this)
}

object ExercisesLibrary {
  private val map = mutable.HashMap[String, Exercise]()

  def apply(name: String) = map(name)
  def get(name: String) = map.get(name)
  def run(name: String, netWrapper: NetWrapper) = map(name).run(netWrapper)
  def add(name: String, exercise: Exercise) = map += (name -> exercise)

  val anyResponseToAnySignal = new Exercise("any response to any signal", 1, List("out1")) {
    def run(wrapper: NetWrapper):Double = {
      var counter = 0
      wrapper.addAfterFire("out1") {
        counter += 1
      }

      wrapper += "1"

      wrapper.tickUntilCalm()
      if (counter > 0) 1.0 else 0.0
    }
  }

  val constantOutputForSixUnits = new Exercise("constant output for six units", 1, List("out1")) {
    def run(wrapper: NetWrapper): Double = {
      var counter = 0
      wrapper.addAfterFire("out1") {
        counter += 1
      }

      wrapper += "1,1,1,1,1,1"

      wrapper.tickUntilCalm()
      if (counter == 6) 1.0 else 0.0
    }
  }

  private val randomResult01Map = mutable.Map[String,Double]()
  val randomResult01 = new Exercise("random result 0-1", 1, List("out1")) {
    def run(wrapper: NetWrapper) = {
      randomResult01Map.getOrElseUpdate(wrapper.net.id, if (Random.nextBoolean()) 1.0 else 0.0)
    }
  }
  
  // here's the list of exercises for DotLine network

  private def dotLinePrepareAndWaitForResult(wrapper: NetWrapper, input: String):(Boolean, Double, Boolean, Double) = {
    var dotFired = false
    var dotResult = 0.0
    wrapper.addAfterFire("dot"){
      dotFired = true
      dotResult = wrapper.lastOutput("dot")
    }

    var lineFired = false
    var lineResult = 0.0
    wrapper.addAfterFire("line"){
      lineFired = true
      lineResult = wrapper.lastOutput("line")
    }

    wrapper.tickUntilCalm(input)

    (dotFired, dotResult, lineFired, lineResult)
  }

  val oneSignalGivesDot = new Exercise("one signal gives dot", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var result = 0.0

      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, "1,0,0")

      if(dotFired){
        result += (if(!lineFired) 2.0 else 1.0)
      }
      if(lineFired) result += 1.0

      result += (if(dotResult <= lineResult) 0.0 else (dotResult - lineResult) * Context().oneSignalGivesDotImportance)

      result
    }
  }

  val twoSignalsGiveLine = new Exercise("two signals give line", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var result = 0.0

      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, "1,1,0")

      if(dotFired) result += 1.0
      if(lineFired){
        result += (if(!dotFired) 2.0 else 1.0)
      }

      result += (if(lineResult <= dotResult) 0.0 else (lineResult - dotResult) * Context().twoSignalsGiveLineImportance)

      result
    }
  }

  val oneSignalWithNoiseGivesDot = new Exercise("one signal with noise gives dot", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var result = 0.0

      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, "1,0,1")

      if(dotFired){
        result += (if(!lineFired) 2.0 else 1.0)
      }
      if(lineFired) result += 1.0

      result += (if(dotResult <= lineResult) 0.0 else (dotResult - lineResult) * Context().oneSignalWithNoiseGivesDotImportance)

      result
    }
  }

  val twoSignalsWithNoiseGiveLine = new Exercise("two signals with noise give line", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var result = 0.0

      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, "1,1,1")

      if(dotFired) result += 1.0
      if(lineFired){
        result += (if(!dotFired) 2.0 else 1.0)
      }

      result += (if(lineResult <= dotResult) 0.0 else (lineResult - dotResult) * Context().twoSignalsWithNoiseGiveLineImportance)

      result
    }
  }

  val oneVariedSignalGivesDot = new Exercise("one varied signal gives dot", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}

      var result = 0.0

      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, List(V, v, v).mkString(","))

      if(dotFired){
        result += (if(!lineFired) 2.0 else 1.0)
      }
      if(lineFired) result += 1.0

      result += (if(dotResult <= lineResult) 0.0 else (dotResult - lineResult) * Context().oneVariedSignalGivesDotImportance)

      result
    }
  }

  val twoVariedSignalsGiveLine = new Exercise("two varied signals give line", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}

      var result = 0.0

      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, List(V, V, v).mkString(","))

      if(dotFired) result += 1.0
      if(lineFired){
        result += (if(!dotFired) 2.0 else 1.0)
      }

      result += (if(lineResult <= dotResult) 0.0 else (lineResult - dotResult) * Context().twoVariedSignalsGiveLineImportance)

      result
    }
  }

  val oneVariedSignalWithNoiseGivesDot = new Exercise("one varied signal with noise gives dot", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}

      var result = 0.0

      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, List(V, v, V).mkString(","))

      if(dotFired){
        result += (if(!lineFired) 2.0 else 1.0)
      }
      if(lineFired) result += 1.0

      result += (if(dotResult <= lineResult) 0.0 else (dotResult - lineResult) * Context().oneVariedSignalWithNoiseGivesDotImportance)

      result
    }
  }

  val twoVariedSignalsWithNoiseGiveLine = new Exercise("two varied signals with noise give line", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}

      var result = 0.0

      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, List(V, V, V).mkString(","))

      if(dotFired) result += 1.0
      if(lineFired){
        result += (if(!dotFired) 2.0 else 1.0)
      }

      result += (if(lineResult <= dotResult) 0.0 else (lineResult - dotResult) * Context().twoVariedSignalsWithNoiseGiveLineImportance)

      result
    }
  }

  // end

}
