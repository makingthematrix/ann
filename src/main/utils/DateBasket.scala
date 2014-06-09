package main.utils

import java.util.Date
import java.util.Calendar
import scala.collection.mutable

trait DateIdentified {
  def date:Date
}

class DateBasket[T <: DateIdentified](val divider:Int) {
  def this() = this(Calendar.DAY_OF_MONTH)
  
  private val map = mutable.Map[Long,mutable.ListBuffer[T]]()
  
  def add(item:T){
    val list = map.getOrElseUpdate(dateTag(item.date),mutable.ListBuffer[T]())
    list += item
  }
  
  def addAll(items: Seq[T]) = items.foreach( add(_) )
  
  def size = map.values.map( _.size ).sum
  
  def get(date: Date) = map.get(dateTag(date)) match {
    case None => None
    case Some(list) => list.find( _.date.equals(date))
  }
		
  def getAllInBasket(date: Date) = map.getOrElse(dateTag(date), mutable.ListBuffer[T]())

  def getClosest(date: Date):Option[T] = map.get(dateTag(date)) match {
    case None => None
    case Some(list) => getClosest(date, list)
  }
  
  private def getClosest(date: Date, seq: Seq[T]):Option[T] = {
    var closest:Option[T] = None
    var difference = Long.MaxValue
    seq.foreach( item => {
      val d = Math.abs(item.date.getTime() - date.getTime())
      if(d < difference){
        closest = Some(item)
        difference = d
      }
    })
    closest
  }
  
  def remove(item: T){
    val tag = dateTag(item.date)
    map.get(tag) match {
      case Some(list) => {
        list -= item
        if(list.isEmpty) map -= tag
      }
      case None =>
    }
  }
  
  def clear = map.clear
	
  def values = map.values.flatten.toSeq
  
  def tagDates = map.keySet.map(tagDate(_))
  
  private def dateTag(date: Date):Long = {
    val cal = Calendar.getInstance()
    cal.setTime(date)
		
	var tag:Long = cal.get(Calendar.YEAR)
	if(divider == Calendar.YEAR) return tag
		
	tag = tag * 12L + cal.get(Calendar.MONTH)
    if(divider == Calendar.MONTH) return tag
		
	tag = tag * 32L + cal.get(Calendar.DAY_OF_MONTH)
	if(divider == Calendar.DAY_OF_MONTH) return tag
		
	tag = tag * 24L + cal.get(Calendar.HOUR_OF_DAY)
	if(divider == Calendar.HOUR_OF_DAY) return tag
	
	tag = tag * 60L + cal.get(Calendar.MINUTE)
	if(divider == Calendar.MINUTE) return tag
		
	tag = tag * 60L + cal.get(Calendar.SECOND)
	if(divider == Calendar.SECOND) return tag
		
	tag = tag * 1000L + cal.get(Calendar.MILLISECOND)
	tag
  }
  
  def tagDate(tag: Long) = {
	val cal = Calendar.getInstance()
	var t = tag	
	if(divider >= Calendar.MILLISECOND){
	  cal.set(Calendar.MILLISECOND, (t % 1000L).toInt)
	  t /= 1000L
	} else cal.set(Calendar.MILLISECOND, 0)			
		
	if(divider >= Calendar.SECOND){
	  cal.set(Calendar.SECOND, (t % 60L).toInt)
	  t /= 60L
	} else cal.set(Calendar.SECOND, 0)
		
	if(divider >= Calendar.MINUTE){
	  cal.set(Calendar.MINUTE, (t % 60L).toInt)
	  t /= 60L
	} else cal.set(Calendar.MINUTE, 0)			
		
	if(divider >= Calendar.HOUR_OF_DAY){
	  cal.set(Calendar.HOUR_OF_DAY, (t % 24L).toInt)
	  t /= 24L
	} else cal.set(Calendar.HOUR_OF_DAY, 0)			
		
	if(divider >= Calendar.DAY_OF_MONTH){
	  cal.set(Calendar.DAY_OF_MONTH, (t % 32L).toInt)
	  t /= 32L
	} else cal.set(Calendar.DAY_OF_MONTH, 1)
		
	if(divider >= Calendar.MONTH){
	  cal.set(Calendar.MONTH, (t % 12).toInt)
	  t /= 12L
	} else cal.set(Calendar.MONTH, 0)
		
	cal.set(Calendar.YEAR, t.toInt)
	cal.getTime()
  }
}

object DateBasket {
  def apply[T <: DateIdentified]() = new DateBasket[T]()
  def apply[T <: DateIdentified](divider: Int) = new DateBasket[T](divider)
  def apply[T <: DateIdentified](divider: Int, items: Seq[T]) = {
    val db = new DateBasket[T](divider)
    db.addAll(items)
  }
}