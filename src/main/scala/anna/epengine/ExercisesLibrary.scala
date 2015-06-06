package anna.epengine

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

  val oneSignalGivesDot = new Exercise("one signal gives dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      var fired = false
      wrapper.addAfterFire("dot"){ fired = true }
      wrapper.tickUntilCalm("1,0,0")
      successIf(fired)
    }
  }

  val oneSignalGivesSomething = new Exercise("one signal gives something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var fired = false
      wrapper.addAfterFire("dot"){ fired = true }
      wrapper.addAfterFire("line"){ fired = true }
      wrapper.tickUntilCalm("1,0,0")
      successIf(fired)
    }
  }

  val oneSignalGivesDotAndSomethingImportance = 10.0

  val oneSignalGivesDotAndSomething = new Exercise("one signal gives dot and something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var dotResult = 0.0
      wrapper.addAfterFire("dot"){ dotResult = wrapper.lastOutput("dot") }
      var lineResult = 0.0
      wrapper.addAfterFire("line"){ lineResult = wrapper.lastOutput("line") }
      wrapper.tickUntilCalm("1,0,0")
      if(dotResult <= lineResult) 0.0 else (dotResult - lineResult) * oneSignalGivesDotAndSomethingImportance
    }
  }

  val oneSignalGivesExactlyOneDot = new Exercise("one signal gives exactly one dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      var counter = 0
      wrapper.addAfterFire("dot") { counter += 1 }
      wrapper.tickUntilCalm("1,0,0")
      successIf(counter == 1)
    }
  }

  val twoSignalsGiveLine = new Exercise("two signals give line", 1, List("line")) {
    def run(wrapper: NetWrapper) = {
      var fired = false
      wrapper.addAfterFire("line") { fired = true }
      wrapper.tickUntilCalm("1,1,0")
      successIf(fired)
    }
  }

  val twoSignalsGiveSomething = new Exercise("two signals give something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var fired = false
      wrapper.addAfterFire("dot") { fired = true }
      wrapper.addAfterFire("line") { fired = true }
      wrapper.tickUntilCalm("1,1,0")
      successIf(fired)
    }
  }

  val twoSignalsGiveLineAndSomethingImportance = 10.0

  val twoSignalsGiveLineAndSomething = new Exercise("two signals give line and something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var dotResult = 0.0
      wrapper.addAfterFire("dot"){ dotResult = wrapper.lastOutput("dot") }
      var lineResult = 0.0
      wrapper.addAfterFire("line"){ lineResult = wrapper.lastOutput("line") }
      wrapper.tickUntilCalm("1,1,0")
      if(lineResult <= dotResult) 0.0 else (lineResult - dotResult) * twoSignalsGiveLineAndSomethingImportance
    }
  }

  val oneSignalWithNoiseGivesDot = new Exercise("one signal with noise gives dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      var fired = false
      wrapper.addAfterFire("dot"){ fired = true }
      wrapper.tickUntilCalm("1,0,1")
      successIf(fired)
    }
  }

  val oneSignalWithNoiseGivesExactlyOneDot = new Exercise("one signal with noise gives exactly one dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      var counter = 0
      wrapper.addAfterFire("dot") { counter += 1 }
      wrapper.tickUntilCalm("1,0,1")
      successIf(counter == 1)
    }
  }

  val oneSignalWithNoiseGivesSomething = new Exercise("one signal with noise gives something", 1, List("dot", "line")) {
    def run(wrapper: NetWrapper) = {
      var fired = false
      wrapper.addAfterFire("dot"){ fired = true }
      wrapper.addAfterFire("line"){ fired = true }
      wrapper.tickUntilCalm("1,0,1")
      successIf(fired)
    }
  }

  val oneSignalWithNoiseGivesDotAndSomethingImportance = 10.0

  val oneSignalWithNoiseGivesDotAndSomething = new Exercise("one signal with noise gives dot and something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var dotResult = 0.0
      wrapper.addAfterFire("dot"){ dotResult = wrapper.lastOutput("dot") }
      var lineResult = 0.0
      wrapper.addAfterFire("line"){ lineResult = wrapper.lastOutput("line") }
      wrapper.tickUntilCalm("1,0,1")
      if(dotResult <= lineResult) 0.0 else (dotResult - lineResult) * oneSignalWithNoiseGivesDotAndSomethingImportance
    }
  }

  val twoSignalsWithNoiseGiveLine = new Exercise("two signals with noise give line", 1, List("line")) {
    def run(wrapper: NetWrapper) = {
      var fired = false
      wrapper.addAfterFire("line") { fired = true }
      wrapper.tickUntilCalm("1,1,1")
      successIf(fired)
    }
  }

  val twoSignalsWithNoiseGiveExactlyOneLine = new Exercise("two signals with noise give exactly one line", 1, List("line")) {
    def run(wrapper: NetWrapper) = {
      var counter = 0
      wrapper.addAfterFire("line") { counter += 1 }
      wrapper.tickUntilCalm("1,1,1")
      successIf(counter == 1)
    }
  }

  val twoSignalsWithNoiseGiveSomething = new Exercise("two signals with noise give something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var fired = false
      wrapper.addAfterFire("dot") { fired = true }
      wrapper.addAfterFire("line") { fired = true }
      wrapper.tickUntilCalm("1,1,1")
      successIf(fired)
    }
  }

  val twoSignalsWithNoiseGiveLineAndSomethingImportance = 10.0

  val twoSignalsWithNoiseGiveLineAndSomething = new Exercise("two signals with noise give line and something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var dotResult = 0.0
      wrapper.addAfterFire("dot"){ dotResult = wrapper.lastOutput("dot") }
      var lineResult = 0.0
      wrapper.addAfterFire("line"){ lineResult = wrapper.lastOutput("line") }
      wrapper.tickUntilCalm("1,1,1")
      if(lineResult <= dotResult) 0.0 else (lineResult - dotResult) * twoSignalsWithNoiseGiveLineAndSomethingImportance
    }
  }

  val oneVariedSignalGivesDot = new Exercise("one varied signal gives dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var fired = false
      wrapper.addAfterFire("dot") { fired = true }
      List(V, v, v).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      successIf(fired)
    }
  }

  val oneVariedSignalGivesSomething = new Exercise("one varied signal gives something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var fired = false
      wrapper.addAfterFire("dot") { fired = true }
      wrapper.addAfterFire("line") { fired = true }
      List(V, v, v).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      successIf(fired)
    }
  }

  val oneVariedSignalGivesDotAndSomethingImportance = 10.0

  val oneVariedSignalGivesDotAndSomething = new Exercise("one varied signal gives dot and something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var dotResult = 0.0
      wrapper.addAfterFire("dot"){ dotResult = wrapper.lastOutput("dot") }
      var lineResult = 0.0
      wrapper.addAfterFire("line"){ lineResult = wrapper.lastOutput("line") }

      List(V, v, v).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      if(dotResult <= lineResult) 0.0 else (dotResult - lineResult) * oneSignalWithNoiseGivesDotAndSomethingImportance
    }
  }

  val oneVariedSignalGivesExactlyOneDot = new Exercise("one varied signal gives exactly one dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("dot") { counter += 1 }
      List(V, v, v).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      successIf(counter == 1)
    }
  }

  val twoVariedSignalsGiveLine = new Exercise("two varied signals give line", 1, List("line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var fired = false
      wrapper.addAfterFire("line") { fired = true }
      List(V, V, v).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      successIf(fired)
    }
  }

  val twoVariedSignalsGiveExactlyOneLine = new Exercise("two varied signals give exactly one line", 1, List("line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("line") { counter += 1 }
      List(V, V, v).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      successIf(counter == 1)
    }
  }

  val twoVariedSignalsGiveLineAndSomethingImportance = 10.0

  val twoVariedSignalsGiveLineAndSomething = new Exercise("two varied signals give line and something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var dotResult = 0.0
      wrapper.addAfterFire("dot"){ dotResult = wrapper.lastOutput("dot") }
      var lineResult = 0.0
      wrapper.addAfterFire("line"){ lineResult = wrapper.lastOutput("line") }

      List(V, V, v).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      if(lineResult <= dotResult) 0.0 else (lineResult - dotResult) * twoVariedSignalsGiveLineAndSomethingImportance
    }
  }

  val oneVariedSignalWithNoiseGivesDot = new Exercise("one varied signal with noise gives dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var fired = false
      wrapper.addAfterFire("dot") { fired = true }
      List(V, v, V).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      successIf(fired)
    }
  }

  val oneVariedSignalWithNoiseGivesExactlyOneDot = new Exercise("one varied signal with noise gives exactly one dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("dot") { counter += 1 }
      List(V, v, V).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      successIf(counter == 1)
    }
  }

  val oneVariedSignalWithNoiseGivesSomething = new Exercise("one varied signal with noise gives something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var fired = false
      wrapper.addAfterFire("dot") { fired = true }
      wrapper.addAfterFire("line") { fired = true }
      List(V, v, V).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      successIf(fired)
    }
  }

  val oneVariedSignalWithNoiseGivesDotAndSomethingImportance = 10.0

  val oneVariedSignalWithNoiseGivesDotAndSomething = new Exercise("one varied signal with noise gives dot and something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var dotResult = 0.0
      wrapper.addAfterFire("dot"){ dotResult = wrapper.lastOutput("dot") }
      var lineResult = 0.0
      wrapper.addAfterFire("line"){ lineResult = wrapper.lastOutput("line") }

      List(V, v, V).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      if(dotResult <= lineResult) 0.0 else (dotResult - lineResult) * oneVariedSignalWithNoiseGivesDotAndSomethingImportance
    }
  }

  val twoVariedSignalsWithNoiseGiveLine = new Exercise("two varied signals with noise give line", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.V
      var fired = false
      wrapper.addAfterFire("line") { fired = true }
      List(V, V, V).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      successIf(fired)
    }
  }

  val twoVariedSignalsWithNoiseGiveExactlyOneLine = new Exercise("two varied signals with noise give exactly one line", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.V
      var counter = 0
      wrapper.addAfterFire("line") { counter += 1 }
      List(V, V, V).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      successIf(counter == 1)
    }
  }

  val twoVariedSignalsWithNoiseGiveSomething = new Exercise("two varied signals with noise give something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.V
      var fired = false
      wrapper.addAfterFire("dot") { fired = true }
      wrapper.addAfterFire("line") { fired = true }
      List(V, V, V).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      successIf(fired)
    }
  }

  val twoVariedSignalsWithNoiseGiveLineAndSomethingImportance = 10.0

  val twoVariedSignalsWithNoiseGiveLineAndSomething = new Exercise("two varied signals with noise give line and something", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.V
      var dotResult = 0.0
      wrapper.addAfterFire("dot"){ dotResult = wrapper.lastOutput("dot") }
      var lineResult = 0.0
      wrapper.addAfterFire("line"){ lineResult = wrapper.lastOutput("line") }
      List(V, V, V).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      if(lineResult <= dotResult) 0.0 else (lineResult - dotResult) * twoVariedSignalsWithNoiseGiveLineAndSomethingImportance
    }
  }

  // end

}
