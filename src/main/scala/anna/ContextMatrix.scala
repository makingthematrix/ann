package anna

import anna.logger.LOG
import anna.utils.DoubleRange

import scala.annotation.tailrec

/**
 * Created by gorywoda on 19.07.15.
 */

case class ContextDoubleRange(varName: String, range: DoubleRange, resolution: Int){
  def iterator = range.iterator(resolution)
}

case class ContextMatrix(drList: List[ContextDoubleRange]){

  private def unfold(myDRList: List[ContextDoubleRange]):List[Map[String,Double]] = myDRList match {
    case Nil => Nil
    case head :: Nil => head.iterator.map(d => Map(head.varName -> d)).toList
    case head :: tail =>
      val foo = unfold(tail)
      head.iterator.flatMap(d => foo.map( _ + (head.varName -> d) ) ).toList
  }

  def unfold:List[Map[String,Double]] = unfold(drList)
}
