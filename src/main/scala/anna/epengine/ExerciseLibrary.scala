package anna.epengine

import anna.async.NetWrapper

/**
 * Created by gorywoda on 11.03.15.
 */
class ExerciseLibrary(map: Map[String, Exercise]){
  def apply(name: String) = map(name)
  def get(name: String) = map.get(name)
  def run(name: String, netWrapper: NetWrapper) = map(name).run(netWrapper)
}

object ExerciseLibrary {
  private var instanceOpt:Option[ExerciseLibrary] = None

  def apply(map: Map[String, Exercise]):ExerciseLibrary = {
    val library = new ExerciseLibrary(map)
    instanceOpt = Some(library)
    library
  }

  def apply():ExerciseLibrary = instanceOpt match {
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


  val map = Map[String, Exercise](
    anyResponseToAnySignal.name -> anyResponseToAnySignal,
    constantOutputForSixUnits.name -> constantOutputForSixUnits
  )

}
