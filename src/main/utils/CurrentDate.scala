package main.utils

import java.util.Calendar

class CurrentDate {
  // this is the only method which should be overridden in subclasses
  def calendar = Calendar.getInstance()
  
  def currentDate = calendar.getTime()
}

object CurrentDate {
  private var _instance:Option[CurrentDate] = None
  
  def apply() = _instance match {
    case Some(date) => date
    case None => {
      val date = new CurrentDate()
      _instance = Some(date)
      date
    }
  }
  
  def set(date: CurrentDate){
    _instance = Some(date)
  }
  
  def reset(){
    _instance = None
  }
}