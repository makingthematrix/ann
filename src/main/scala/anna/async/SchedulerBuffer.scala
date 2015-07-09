package anna.async

import akka.actor.{ActorContext, Cancellable}

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
 * Created by gorywoda on 25.06.15.
 */
class SchedulerBuffer(context : ActorContext){
  private val map = mutable.Map[Long,Cancellable]()

  def schedule(delay : FiniteDuration)(f : => scala.Unit) = {
    refresh()
    val c = context.system.scheduler.scheduleOnce(delay){
      f
      refresh()
    }
    val timestamp = System.currentTimeMillis() + delay.toMillis
    map += (timestamp -> c)
    timestamp
  }

  def unschedule(timestamp: Long) = if(map.contains(timestamp)){
    refresh()
    val c = map(timestamp)
    if(!c.isCancelled) c.cancel()
    map -= timestamp
  }

  def refresh() = {
    val t = System.currentTimeMillis()
    map.keys.filter(_ < t).foreach( map -= _)
  }

  def clear() = {
    map.values.foreach( c => if(!c.isCancelled) c.cancel())
    map.clear()
  }
}
