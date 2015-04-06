package anna.epengine

import anna.async.NetWrapper
import scala.util.Random

/**
 * Created by gorywoda on 11.03.15.
 */
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

  val anyResponseToAnySignal = Exercise("any response to any signal", 1, List("out1"),
                                        (wrapper: NetWrapper, good: Double, bad: Double) => {
    var counter = 0
    wrapper.addAfterFire("out1"){ counter += 1 }

    wrapper += "1"

    wrapper.tickUntilCalm()
    if(counter > 0) good else bad
  })

  val constantOutputForSixUnits = Exercise("constant output for six units", 1, List("out1"),
                                           (wrapper: NetWrapper, success: Double, failure: Double) => {
    var counter = 0
    wrapper.addAfterFire("out1"){ counter += 1 }

    wrapper += "1,1,1,1,1,1"

    wrapper.tickUntilCalm()
    if(counter == 6) success else failure
  })

  val randomResult01 = Exercise("random result 0-1", 1, List("out1"),
                                (wrapper: NetWrapper, success: Double, failure: Double) => {
    if(Random.nextBoolean()) success else failure
  })

  val oneSignalGivesDot = Exercise("one signal gives dot", 1, List("dot"),
                                  (wrapper: NetWrapper, success: Double, failure: Double) => {
    var counter = 0
    wrapper.addAfterFire("dot"){ counter += 1 }
    wrapper += "1,0,0"
    wrapper.tickUntilCalm()
    if(counter > 0) success else failure
  })

  val oneSignalGivesExactlyOneDot = Exercise("one signal gives exactly one dot", 1, List("dot"),
    (wrapper: NetWrapper, success: Double, failure: Double) => {
      var counter = 0
      wrapper.addAfterFire("dot"){ counter += 1 }
      wrapper += "1,0,0"
      wrapper.tickUntilCalm()
      if(counter == 1) success else failure
    })

  val twoSignalsGiveNothing = Exercise("two signals give nothing", 1, List("dot"),
    (wrapper: NetWrapper, success: Double, failure: Double) => {
      var counter = 0
      wrapper.addAfterFire("dot"){ counter += 1 }
      wrapper += "1,1,0"
      wrapper.tickUntilCalm()
      if(counter == 0) success else failure
    })

  val oneSignalWithNoiseGivesDot = Exercise("one signal with noise gives dot", 1, List("dot"),
    (wrapper: NetWrapper, success: Double, failure: Double) => {
      var counter = 0
      wrapper.addAfterFire("dot"){ counter += 1 }
      wrapper += "1,0,1"
      wrapper.tickUntilCalm()
      if(counter > 0) success else failure
    })

  val oneSignalWithNoiseGivesExactlyOneDot = Exercise("one signal with noise gives exactly one dot", 1, List("dot"),
    (wrapper: NetWrapper, success: Double, failure: Double) => {
      var counter = 0
      wrapper.addAfterFire("dot"){ counter += 1 }
      wrapper += "1,0,1"
      wrapper.tickUntilCalm()
      if(counter == 1) success else failure
    })

  val twoSignalsWithNoiseGiveNothing = Exercise("two signals with noise give nothing", 1, List("dot"),
    (wrapper: NetWrapper, success: Double, failure: Double) => {
      var counter = 0
      wrapper.addAfterFire("dot"){ counter += 1 }
      wrapper += "1,1,1"
      wrapper.tickUntilCalm()
      if(counter == 0) success else failure
    })

  val oneVariedSignalGivesDot = Exercise("one varied signal gives dot", 1, List("dot"),
    (wrapper: NetWrapper, success: Double, failure: Double) => {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("dot"){ counter += 1 }
      List(V,v,v).foreach( wrapper += _ )
      wrapper.tickUntilCalm()
      if(counter > 0) success else failure
    })

  val oneVariedSignalGivesExactlyOneDot = Exercise("one varied signal gives exactly one dot", 1, List("dot"),
    (wrapper: NetWrapper, success: Double, failure: Double) => {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("dot"){ counter += 1 }
      List(V,v,v).foreach( wrapper += _ )
      wrapper.tickUntilCalm()
      if(counter == 1) success else failure
    })

  val twoVariedSignalsGiveNothing = Exercise("two varied signals give nothing", 1, List("dot"),
    (wrapper: NetWrapper, success: Double, failure: Double) => {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("dot"){ counter += 1 }
      List(V,V,v).foreach( wrapper += _ )
      wrapper.tickUntilCalm()
      if(counter == 0) success else failure
    })

  val oneVariedSignalWithNoiseGivesDot = Exercise("one varied signal with noise gives dot", 1, List("dot"),
    (wrapper: NetWrapper, success: Double, failure: Double) => {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("dot"){ counter += 1 }
      List(V,v,V).foreach( wrapper += _ )
      wrapper.tickUntilCalm()
      if(counter > 0) success else failure
    })

  val oneVariedSignalWithNoiseGivesExactlyOneDot = Exercise("one varied signal with noise gives exactly one dot", 1, List("dot"),
    (wrapper: NetWrapper, success: Double, failure: Double) => {
      import anna.utils.Utils.{V, v}
      var counter = 0
      wrapper.addAfterFire("dot"){ counter += 1 }
      List(V,v,V).foreach( wrapper += _ )
      wrapper.tickUntilCalm()
      if(counter == 1) success else failure
    })

  val twoVariedSignalsWithNoiseGiveNothing = Exercise("two varied signals with noise give nothing", 1, List("dot"),
    (wrapper: NetWrapper, success: Double, failure: Double) => {
      import anna.utils.Utils.V
      var counter = 0
      wrapper.addAfterFire("dot"){ counter += 1 }
      List(V,V,V).foreach( wrapper += _ )
      wrapper.tickUntilCalm()
      if(counter == 0) success else failure
    })

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
