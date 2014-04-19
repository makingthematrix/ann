package main

object Utils {
  def assert(str: String):IllegalArgumentException = throw new IllegalArgumentException(str)
  def assert(condition: => Boolean, str: String):Unit = if(condition) assert(str)
}