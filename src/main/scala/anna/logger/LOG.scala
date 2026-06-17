package anna.logger

import java.io.{PrintWriter, StringWriter}
import anna.utils.Utils
import anna.async.Neuron

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object LogLevel extends Enumeration {
  type LogLevel = Value
  val NONE, ERROR, DEBUG, INFO, COMMENT, ALL = Value
}

object LOG {
  private var logLevel: LogLevel.Value = LogLevel.DEBUG
  private val outs: ListBuffer[LogOutput] = new mutable.ListBuffer[LogOutput]()
  private var logTime: Boolean = true
  private var showLogLevel: Boolean = false
  private var trackAll: Boolean = true

  private var fetchIterationFunction:Option[() => Int] = None

  private def logIteration: Boolean = fetchIterationFunction.isDefined

  def startLoggingIterations(f:() => Int): Unit = {
    fetchIterationFunction = Some(f)
  }

  def stopLogingIterations(): Unit = {
    fetchIterationFunction = None
  }

  private val trackedClasses = mutable.Set[String]()

  def track(c: Class[_]): Unit = {
    val className = c.getName
    trackedClasses += (if(className.endsWith("$")) className.substring(0,className.size-1) else className)
    trackAll = false
  }

  def stopTracking(c: Class[_]): Unit = trackedClasses -= c.getName

  def isTracking(className: String):Boolean = trackedClasses.contains(className)

  private def isTracking(c: Class[_]):Boolean = trackedClasses.contains(c.getName)

  def resetTracked(): Unit = {
    trackedClasses.clear()
    trackAll = true
  }

  def resetOuts(): Unit = this.synchronized {
    outs.foreach( _.close() )
    outs.clear()
    outs += new SystemLogOutput()
  }

  def findOut(id: String): Option[LogOutput] = this.synchronized {
    outs.find( _.id == id )
  }

  def addOut(out: LogOutput): LogOutput = findOut(out.id) match {
    case Some(o) => o
    case None => outs += out; out
  }

  private val _allowedIds = mutable.Set[String]()

  def allow(id: String):Unit = _allowedIds += id
  def allow(ids: String*):Unit = ids.foreach( allow )
  def allow[N <: Neuron](implicit n: N):Unit = allow(n.id)

  def allowedIds: Set[String] = _allowedIds.toSet
  def clearAllowedIds(): Unit = _allowedIds.clear()
  def removeAllowedId(id: String): Unit = _allowedIds -= id
  def allowed(id: String):Boolean = _allowedIds.contains(id)
  def allowed[N <: Neuron](n: N):Boolean = allowed(n.id)

  def +=(str: String)(implicit n: Neuron): Unit = log(str, n)
  def log(str: String, n: Neuron):Unit = if(allowed(n)) log(n.id + ": " + str, logLevel)

  def removeOut(out: LogOutput): Boolean = findOut(out.id) match{
    case None => false;
    case Some(o) => o.close(); outs -= o; true
  }

  def addLogToHTML(id: String): LogOutput = addOut(new HTMLLogOutput(id))

  def addLogToFile(fileName: String): LogOutput = addOut(new FileLogOutput(fileName))

  def addLogToStdout(): LogOutput = addOut(new StdoutLogOutput())

  def log(str: String, logLevel: LogLevel.Value):Unit = this.synchronized {
    if(logLevel > this.logLevel) return
    val sb = new StringBuilder()
    if(showLogLevel) sb ++= "level " + logLevel.toString() + ':'
    if(logTime) sb ++= "time " + dateTag + ':'
    if(logIteration) sb++= "iter " + fetchIterationFunction.get().toString + ':'
    sb ++= ">\t"
    sb ++= str
    outs.foreach{ _.println(sb.toString) }
  }
  
  private var offset:Option[Long] = None

  def timer(): Unit = {
    offset = Some(System.currentTimeMillis())
  }

  def date(): Unit = {
    offset = None
  }

  def time: Long = offset match {
    case Some(t) => (System.currentTimeMillis() - t)
    case None => throw new IllegalArgumentException("Logger.time called with no timer set")
  }
  
  def timerSet: Boolean = offset.isDefined

  private def dateTag = offset match {
    case None => Utils.dateTag
    case Some(t) => (System.currentTimeMillis() - t).toString
  }
	
  def log(expr: => Boolean, str: String, logLevel: LogLevel.Value):Unit = if(expr) log(str, logLevel)
	
  def log(source: Any, str: String, logLevel: LogLevel.Value, neuronAllowed: Boolean =false):Unit = 
    if(trackAll || neuronAllowed || isTracking(source.getClass))
	  log(source.getClass.getName + "->" + str, logLevel)
	

  def stackToString(t: Throwable): String = {
	  val result = new StringWriter()
	  t.printStackTrace(new PrintWriter(result))
	  result.toString
  }

  def exception(t: Throwable): Unit = {
	  log("Throwable object caught of type " + t.getClass.getName, LogLevel.ERROR)
	  log("message: " + t.getMessage, LogLevel.ERROR)
	  log("cause: " + t.getCause, LogLevel.ERROR)
	  log(stackToString(t), LogLevel.ERROR)
	  throw new IllegalArgumentException(t.getMessage)
  }
	
  def error(t: Throwable): Unit = {
	  error("Throwable object caught of type " + t.getClass.getName)
	  error("message: " + t.getMessage)
	  error("cause: " + t.getCause)
	  error(stackToString(t))
  }

  def exception(str: String): Unit = {
	  log(str, LogLevel.ERROR)
	  throw new IllegalArgumentException(str)
  }
	
  def error(str: String): Unit = if (LogLevel.ERROR <= logLevel) log(str, LogLevel.ERROR)
  def debug(str: String): Unit = if (LogLevel.ERROR <= logLevel) log(str, LogLevel.DEBUG)
  def info(str: String): Unit = if (LogLevel.ERROR <= logLevel) log(str, LogLevel.INFO)
  def comment(str: String): Unit = if (LogLevel.ERROR <= logLevel) log(str, LogLevel.COMMENT)

  def exception(expr: => Boolean, str: String): Unit = {
	  log(expr, str, LogLevel.ERROR)
	  if(expr) throw new IllegalArgumentException(str)
  }
	
  def error(expr: => Boolean, str: String): Unit = if(LogLevel.ERROR <= logLevel) log(expr, str, LogLevel.ERROR)
  def debug(expr: => Boolean, str: String): Unit = if(LogLevel.ERROR <= logLevel) log(expr, str, LogLevel.DEBUG)
  def info(expr: => Boolean, str: String): Unit = if(LogLevel.ERROR <= logLevel) log(expr, str, LogLevel.INFO)
  def comment(expr: => Boolean, str: String): Unit = if(LogLevel.ERROR <= logLevel) log(expr, str, LogLevel.COMMENT)

  def exception(source: Any, str: String): Unit = {
	  log(source, str, LogLevel.ERROR)
	  throw new IllegalArgumentException(str)
  }

  def error[N <: Neuron](source: N, str: String): Unit = if(LogLevel.ERROR <= logLevel) log(source, str, LogLevel.ERROR, allowed(source))
  def debug[N <: Neuron](source: N, str: String): Unit = if(LogLevel.ERROR <= logLevel) log(source, str, LogLevel.DEBUG, allowed(source))
  def info[N <: Neuron](source: N, str: String): Unit = if(LogLevel.ERROR <= logLevel) log(source, str, LogLevel.INFO, allowed(source))
  def comment[N <: Neuron](source: N, str: String): Unit = if(LogLevel.ERROR <= logLevel) log(source, str, LogLevel.COMMENT, allowed(source))

  def error(source: Any, str: String): Unit = if(LogLevel.ERROR <= logLevel) log(source, str, LogLevel.ERROR)
  def debug(source: Any, str: String): Unit = if(LogLevel.ERROR <= logLevel) log(source, str, LogLevel.DEBUG)
  def info(source: Any, str: String): Unit = if(LogLevel.ERROR <= logLevel) log(source, str, LogLevel.INFO)
  def comment(source: Any, str: String): Unit = if(LogLevel.ERROR <= logLevel) log(source, str, LogLevel.COMMENT)

  def log(str: String): Unit = log(str, logLevel)
  def log(expr: => Boolean, str: String): Unit = log(expr, str, logLevel)
  def log(source: Any, str: String): Unit = log(source, str, logLevel)
	
  private var lastMeasure: Long = System.currentTimeMillis

  def timer(tag: String): Long = {
	  val t = System.currentTimeMillis
	  val result = t - lastMeasure
	  log(tag + " took " + result + "ms.")
	  lastMeasure = t
	  result
  }

  def timer(source: Any, tag: String): Long = timer(source.getClass.getName + "->" + tag)

  def resetTimer(): Unit = {
	  lastMeasure = System.currentTimeMillis()
  }
}
