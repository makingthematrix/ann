package anna.epengine

import anna.Context
import anna.async.NetWrapper
import anna.logger.LOG

import scala.util.Random
import scala.collection.mutable

/**
 * Created by gorywoda on 11.03.15.
 */

abstract class Exercise(val name: String, val inputLen: Int, val outputIds: List[String]) {
  def run(wrapper: NetWrapper): Double
  def successIf(flag: Boolean) = if(flag) 1.0 else 0.0

  ExercisesLibrary.add(this)
}

object ExercisesLibrary {
  private val map = mutable.HashMap[String, Exercise]()

  def apply(name: String) = map(name)
  def get(name: String) = map.get(name)
  def run(name: String, netWrapper: NetWrapper) = map(name).run(netWrapper)
  def add(exercise: Exercise) = map += (exercise.name -> exercise)

  val anyResponseToAnySignal = new Exercise("any response to any signal", 1, List("out1")) {
    def run(wrapper: NetWrapper):Double = {
      var counter = 0
      wrapper.addAfterFire("out1")( (_:Double) => {
        counter += 1
      })

      wrapper += "1"

      wrapper.tickUntilCalm()
      if (counter > 0) 1.0 else 0.0
    }
  }

  val constantOutputForSixUnits = new Exercise("constant output for six units", 1, List("out1")) {
    def run(wrapper: NetWrapper): Double = {
      var counter = 0
      wrapper.addAfterFire("out1")( (_:Double) => {
        counter += 1
      })

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
    wrapper.addAfterFire("dot")( (output: Double) => {
      dotFired = true
      dotResult = output //wrapper.lastOutput("dot")
    })

    var lineFired = false
    var lineResult = 0.0
    wrapper.addAfterFire("line")( (output: Double) => {
      lineFired = true
      lineResult = output //wrapper.lastOutput("line")
    })

    wrapper.tickUntilCalm(input)

    (dotFired, dotResult, lineFired, lineResult)
  }

  private def dotLineCountFires(wrapper: NetWrapper, input: String):(Int, Int) = {
    var dotFired = 0
    wrapper.addAfterFire("dot")( (_: Double) => { dotFired += 1 })
    var lineFired = 0
    wrapper.addAfterFire("line")( (_: Double) => { lineFired += 1 })

    wrapper.tickUntilCalm(input)

    wrapper.removeAllTriggers()

    (dotFired, lineFired)
  }

  private def countFires(wrapper: NetWrapper, fires: Int, weight: Double, dotFireCoeff: Double = 10.0, lineFireCoeff: Double = 10.0, successCoeff: Double = 5.0) = {
    var result = 0.0

    val (dotDot, lineDot) = dotLineCountFires(wrapper, (0 until fires).map(n => "1,0,0").mkString(","))
    val (dotLine, lineLine) = dotLineCountFires(wrapper, (0 until fires).map(n => "1,1,0").mkString(","))
    LOG.debug(s"countFires$fires: dotDot: $dotDot, lineDot: $lineDot, dotLine: $dotLine, lineLine: $lineLine")

    if (dotDot == fires) result += dotFireCoeff * weight
    else if (dotDot > fires) result += weight
    else if(dotDot == 0){
      val info = wrapper.info("dot")
      result += weight * dotFireCoeff * 0.5 * (1.0 - info.threshold + info.highestBuffer)
    }
    result -= dotLine * dotFireCoeff * 0.5 * weight

    if (lineLine == fires) result += lineFireCoeff * weight
    else if (lineLine > fires) result += weight
    else if(lineLine == 0){
      val info = wrapper.info("line")
      result += weight * lineFireCoeff * 0.5 * (1.0 - info.threshold + info.highestBuffer)
    }
    result -= lineDot * lineFireCoeff * 0.5 * weight

    if (dotDot == fires && lineLine == fires && dotLine < fires && lineDot < fires) result += successCoeff * weight

    result
  }

  val countFiresEqually1 = new Exercise("count fires equally 1", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = countFires(wrapper, 1, 3.0)
  }

  val countFiresEqually2 = new Exercise("count fires equally 2", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = countFires(wrapper, 2, 0.5)
  }

  val countFiresEqually3 = new Exercise("count fires equally 3", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = countFires(wrapper, 3, 0.1)
  }

  val countFiresPreferDot1 = new Exercise("count fires prefer dot 1", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = countFires(wrapper, 1, 3.0, 10.0, 5.0)
  }

  val countFiresPreferDot2 = new Exercise("count fires prefer dot 2", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = countFires(wrapper, 2, 0.5, 10.0, 5.0)
  }

  val countFiresPreferDot3 = new Exercise("count fires prefer dot 3", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = countFires(wrapper, 3, 0.1, 10.0, 5.0)
  }

  val countFiresPreferLine1 = new Exercise("count fires prefer line 1", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = countFires(wrapper, 1, 3.0, 5.0, 10.0)
  }

  val countFiresPreferLine2 = new Exercise("count fires prefer line 2", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = countFires(wrapper, 2, 0.5, 5.0, 10.0)
  }

  val countFiresPreferLine3 = new Exercise("count fires prefer line 3", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = countFires(wrapper, 3, 0.1, 5.0, 10.0)
  }


  // ----

  val oneForAll = new Exercise("one for all", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var result = 0.0

      val (dotDot1, lineDot1) = dotLineCountFires(wrapper, "1,0,0")
      val (dotLine1, lineLine1) = dotLineCountFires(wrapper, "1,1,0")
      LOG.debug(s"${this.name}: dotDot1: $dotDot1, lineDot1: $lineDot1, dotLine1: $dotLine1, lineLine1: $lineLine1")

      if(dotDot1 == 1) result += 30.0
      else if(dotDot1 > 1) result += 3.0
      result -= dotLine1*3.0
      if(lineLine1 == 1) result += 30.0
      else if(lineLine1 > 1) result += 3.0
      result -= lineDot1*3.0
      if(dotDot1 == 1 && lineLine1 == 1) result += 60.0

      val (dotDot2, lineDot2) = dotLineCountFires(wrapper, "1,0,0,1,0,0")
      val (dotLine2, lineLine2) = dotLineCountFires(wrapper, "1,1,0,1,1,0")
      LOG.debug(s"${this.name}: dotDot2: $dotDot2, lineDot2: $lineDot2, dotLine2: $dotLine2, lineLine2: $lineLine2")

      if(dotDot2 == 2) result += 20.0
      else if(dotDot2 == 1) result += 2.0
      result -= dotLine2*2.0
      if(lineLine2 == 2) result += 20.0
      else if(lineLine2 == 1) result += 2.0
      result -= lineDot2*2.0
      if(dotDot2 == 1 && lineLine2 == 1) result += 40.0

      val (dotDot3, lineDot3) = dotLineCountFires(wrapper, "1,0,0,1,0,0,1,0,0")
      val (dotLine3, lineLine3) = dotLineCountFires(wrapper, "1,1,0,1,1,0,1,1,0")
      LOG.debug(s"${this.name}: dotDot3: $dotDot3, lineDot3: $lineDot3, dotLine3: $dotLine3, lineLine3: $lineLine3")

      if(dotDot3 == 3) result += 10.0
      else if(dotDot3 == 1 || dotDot3 == 2) result += 1.0
      result -= dotLine3*1.0
      if(lineLine3 == 3) result += 10.0
      else if(lineLine3 == 1 || lineLine3 == 2) result += 1.0
      result -= lineDot3*1.0
      if(dotDot3 == 1 && lineLine3 == 1) result += 20.0

      result
    }
  }

  val oneSignalGivesDot = new Exercise("one signal gives dot", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var result = 0.0

      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, "1,0,0")
      LOG.debug(this,s"${this.name}: dotFired: $dotFired, dotResult: $dotResult, lineFired: $lineFired, lineResult: $lineResult")

      if(dotFired){
        result += (if(!lineFired) 10.0 else 5.0)
      }

      result += (if(dotResult <= lineResult) 0.0 else (dotResult - lineResult) * Context().oneSignalGivesDotImportance)

      result
    }
  }

  val twoSignalsGiveLine = new Exercise("two signals give line", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var result = 0.0

      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, "1,1,0")

      /*if(dotFired) result += 1.0
      if(lineFired){
        result += (if(!dotFired) 2.0 else 1.0)
      }*/
      if(lineFired){
        result += (if(!dotFired) 10.0 else 5.0)
      }

      result += (if(lineResult <= dotResult) 0.0 else (lineResult - dotResult) * Context().twoSignalsGiveLineImportance)

      result
    }
  }

  val oneSignalWithNoiseGivesDot = new Exercise("one signal with noise gives dot", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var result = 0.0

      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, "1,0,1")

      /*if(dotFired){
        result += (if(!lineFired) 2.0 else 1.0)
      }
      if(lineFired) result += 1.0*/
      if(dotFired){
        result += (if(!lineFired) 10.0 else 5.0)
      }

      result += (if(dotResult <= lineResult) 0.0 else (dotResult - lineResult) * Context().oneSignalWithNoiseGivesDotImportance)

      result
    }
  }

  val twoSignalsWithNoiseGiveLine = new Exercise("two signals with noise give line", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      var result = 0.0

      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, "1,1,1")

      /*if(dotFired) result += 1.0
      if(lineFired){
        result += (if(!dotFired) 2.0 else 1.0)
      }*/
      if(lineFired){
        result += (if(!dotFired) 10.0 else 5.0)
      }

      result += (if(lineResult <= dotResult) 0.0 else (lineResult - dotResult) * Context().twoSignalsWithNoiseGiveLineImportance)

      result
    }
  }

  val oneVariedSignalGivesDot = new Exercise("one varied signal gives dot", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}

      var result = 0.0

      wrapper.regSign('a',V); wrapper.regSign('b',v); wrapper.regSign('c',v);
      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, "a,b,c")
      List('a','b','c').foreach( wrapper.deregSign )

      /*if(dotFired){
        result += (if(!lineFired) 2.0 else 1.0)
      }
      if(lineFired) result += 1.0*/
      if(dotFired){
        result += (if(!lineFired) 10.0 else 5.0)
      }

      result += (if(dotResult <= lineResult) 0.0 else (dotResult - lineResult) * Context().oneVariedSignalGivesDotImportance)

      result
    }
  }

  val twoVariedSignalsGiveLine = new Exercise("two varied signals give line", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}

      var result = 0.0

      wrapper.regSign('a',V); wrapper.regSign('b',V); wrapper.regSign('c',v);
      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, "a,b,c")
      List('a','b','c').foreach( wrapper.deregSign )

      /*if(dotFired) result += 1.0
      if(lineFired){
        result += (if(!dotFired) 2.0 else 1.0)
      }*/
      if(lineFired){
        result += (if(!dotFired) 10.0 else 5.0)
      }

      result += (if(lineResult <= dotResult) 0.0 else (lineResult - dotResult) * Context().twoVariedSignalsGiveLineImportance)

      result
    }
  }

  val oneVariedSignalWithNoiseGivesDot = new Exercise("one varied signal with noise gives dot", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}

      var result = 0.0

      wrapper.regSign('a',V); wrapper.regSign('b',v); wrapper.regSign('c',V);
      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, "a,b,c")
      List('a','b','c').foreach( wrapper.deregSign )

      /*if(dotFired){
        result += (if(!lineFired) 2.0 else 1.0)
      }
      if(lineFired) result += 1.0*/
      if(dotFired){
        result += (if(!lineFired) 10.0 else 5.0)
      }

      result += (if(dotResult <= lineResult) 0.0 else (dotResult - lineResult) * Context().oneVariedSignalWithNoiseGivesDotImportance)

      result
    }
  }

  val twoVariedSignalsWithNoiseGiveLine = new Exercise("two varied signals with noise give line", 1, List("dot","line")) {
    def run(wrapper: NetWrapper) = {
      import anna.utils.Utils.{V, v}

      var result = 0.0

      wrapper.regSign('a',V); wrapper.regSign('b',V); wrapper.regSign('c',V);
      val (dotFired, dotResult, lineFired, lineResult) = dotLinePrepareAndWaitForResult(wrapper, "a,b,c")
      List('a','b','c').foreach( wrapper.deregSign )

      /*if(dotFired) result += 1.0
      if(lineFired){
        result += (if(!dotFired) 2.0 else 1.0)
      }*/
      if(lineFired){
        result += (if(!dotFired) 10.0 else 5.0)
      }

      result += (if(lineResult <= dotResult) 0.0 else (lineResult - dotResult) * Context().twoVariedSignalsWithNoiseGiveLineImportance)

      result
    }
  }

  // end

}
