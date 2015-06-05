package anna.epengine

import anna.async.NetWrapper

import scala.util.Random
import scala.collection.mutable

/**
 * Created by gorywoda on 11.03.15.
 */

abstract class Exercise(val name: String, val inputLen: Int, val outputIds: List[String]) {
  def run(wrapper: NetWrapper): Double
}

class ExercisesLibrary(map: Map[String, Exercise]){
  def apply(name: String) = map(name)
  def get(name: String) = map.get(name)
  def run(name: String, netWrapper: NetWrapper) = map(name).run(netWrapper)
}

object ExercisesLibrary {
  private var instanceOpt:Option[ExercisesLibrary] = None

  def apply(map: Map[String, Exercise]):ExercisesLibrary = {
    val library = new ExercisesLibrary(map)
    instanceOpt = Some(library)
    library
  }

  def apply():ExercisesLibrary = instanceOpt match {
    case None => apply(map)
    case Some(instance) => instance
  }

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

  val oneSignalGivesDot = new Exercise("one signal gives dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      var counter = 0
      wrapper.addAfterFire("dot") {
        counter += 1
      }
      wrapper += "1,0,0"
      wrapper.tickUntilCalm()
      if (counter > 0) 1.0 else 0.0
    }
  }

  val oneSignalGivesExactlyOneDot = new Exercise("one signal gives exactly one dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      var counter = 0
      wrapper.addAfterFire("dot") {
        counter += 1
      }
      wrapper += "1,0,0"
      wrapper.tickUntilCalm()
      if (counter == 1) 1.0 else 0.0
    }
  }

  val twoSignalsGiveNothing = new Exercise("two signals give nothing", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      var counter = 0
      wrapper.addAfterFire("dot") {
        counter += 1
      }
      wrapper += "1,1,0"
      wrapper.tickUntilCalm()
      if (counter == 0) 1.0 else 0.0
    }
  }

  val oneSignalWithNoiseGivesDot = new Exercise("one signal with noise gives dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      var counter = 0
      wrapper.addAfterFire("dot") {
        counter += 1
      }
      wrapper += "1,0,1"
      wrapper.tickUntilCalm()
      if (counter > 0) 1.0 else 0.0
    }
  }

  val oneSignalWithNoiseGivesExactlyOneDot = new Exercise("one signal with noise gives exactly one dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      var counter = 0
      wrapper.addAfterFire("dot") {
        counter += 1
      }
      wrapper += "1,0,1"
      wrapper.tickUntilCalm()
      if (counter == 1) 1.0 else 0.0
    }
  }

  val twoSignalsWithNoiseGiveNothing = new Exercise("two signals with noise give nothing", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      var counter = 0
      wrapper.addAfterFire("dot") {
        counter += 1
      }
      wrapper += "1,1,1"
      wrapper.tickUntilCalm()
      if (counter == 0) 1.0 else 0.0
    }
  }

  val oneVariedSignalGivesDot = new Exercise("one varied signal gives dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("dot") {
        counter += 1
      }
      List(V, v, v).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      if (counter > 0) 1.0 else 0.0
    }
  }

  val oneVariedSignalGivesExactlyOneDot = new Exercise("one varied signal gives exactly one dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("dot") {
        counter += 1
      }
      List(V, v, v).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      if (counter == 1) 1.0 else 0.0
    }
  }

  val twoVariedSignalsGiveNothing = new Exercise("two varied signals give nothing", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("dot") {
        counter += 1
      }
      List(V, V, v).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      if (counter == 0) 1.0 else 0.0
    }
  }

  val oneVariedSignalWithNoiseGivesDot = new Exercise("one varied signal with noise gives dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("dot") {
        counter += 1
      }
      List(V, v, V).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      if (counter > 0) 1.0 else 0.0
    }
  }

  val oneVariedSignalWithNoiseGivesExactlyOneDot = new Exercise("one varied signal with noise gives exactly one dot", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("dot") {
        counter += 1
      }
      List(V, v, V).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      if (counter == 1) 1.0 else 0.0
    }
  }

  val twoVariedSignalsWithNoiseGiveNothing = new Exercise("two varied signals with noise give nothing", 1, List("dot")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.V
      var counter = 0
      wrapper.addAfterFire("dot") {
        counter += 1
      }
      List(V, V, V).foreach(wrapper += _)
      wrapper.tickUntilCalm()
      if (counter == 0) 1.0 else 0.0
    }
  }

  val map = Map[String, Exercise](
        anyResponseToAnySignal.name -> anyResponseToAnySignal,
        constantOutputForSixUnits.name -> constantOutputForSixUnits,
        randomResult01.name -> randomResult01,
        oneSignalGivesDot.name -> oneSignalGivesDot,
        oneSignalGivesExactlyOneDot.name -> oneSignalGivesExactlyOneDot,
        twoSignalsGiveNothing.name -> twoSignalsGiveNothing,
        oneSignalWithNoiseGivesDot.name -> oneSignalWithNoiseGivesDot,
        oneSignalWithNoiseGivesExactlyOneDot.name -> oneSignalWithNoiseGivesExactlyOneDot,
        twoSignalsWithNoiseGiveNothing.name -> twoSignalsWithNoiseGiveNothing,
        oneVariedSignalGivesDot.name -> oneVariedSignalGivesDot,
        oneVariedSignalGivesExactlyOneDot.name -> oneVariedSignalGivesExactlyOneDot,
        twoVariedSignalsGiveNothing.name -> twoVariedSignalsGiveNothing,
        oneVariedSignalWithNoiseGivesDot.name -> oneVariedSignalWithNoiseGivesDot,
        oneVariedSignalWithNoiseGivesExactlyOneDot.name -> oneVariedSignalWithNoiseGivesExactlyOneDot,
        twoVariedSignalsWithNoiseGiveNothing.name -> twoVariedSignalsWithNoiseGiveNothing
  )


}
