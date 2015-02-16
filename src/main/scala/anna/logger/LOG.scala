package anna.logger

import java.io.{PrintWriter, StringWriter}
import java.util.Calendar

import anna.async.Neuron

import scala.collection.mutable

object LogLevel extends Enumeration {
  type LogLevel = Value
  val NONE, ERROR, DEBUG, INFO, COMMENT, ALL = Value
}

object LOG {
  var logLevel = LogLevel.DEBUG 
  val outs = new mutable.ListBuffer[LogOutput]()
  var logDate = true
  var showLogLevel = false
  var trackAll = true
  
  private val trackedClasses = mutable.Set[String]()
  
  def track(c: Class[_]) = {
    val className = c.getName()
    trackedClasses += (if(className.endsWith("$")) className.substring(0,className.size-1) else className)
    trackAll = false
  }
  
  def stopTracking(c: Class[_]) = trackedClasses -= c.getName
  
  def isTracking(className: String):Boolean = trackedClasses.contains(className)
  def isTracking(c: Class[_]):Boolean = trackedClasses.contains(c.getName)
  
  def resetTracked() = {
    trackedClasses.clear
    trackAll = true
  }

  def resetOuts() = this.synchronized {
    outs.foreach( _.close )
    outs.clear
    outs += new SystemLogOutput()
  }
  
  def findOut(id: String) = this.synchronized {
    outs.find( _.id == id )
  }

  def addOut(out: LogOutput) = findOut(out.id) match {
    case Some(o) => o
    case None => outs += out; out
  }
  
  private val _allowedIds = mutable.Set[String]()
  
  def allow(id: String):Unit = _allowedIds += id
  def allow(ids: String*):Unit = ids.foreach( allow(_) )
  def allow[N <: Neuron](implicit n: N):Unit = allow(n.id)
  
  def allowedIds = _allowedIds.toSet
  def clearAllowedIds() = _allowedIds.clear()
  def removeAllowedId(id: String) = _allowedIds -= id
  def allowed(id: String):Boolean = _allowedIds.contains(id)
  def allowed[N <: Neuron](n: N):Boolean = allowed(n.id)
  
  def +=(str: String)(implicit n: Neuron) = log(str, n)
  def log(str: String, n: Neuron):Unit = if(allowed(n)) log(str, logLevel)
  
  def removeOut(out: LogOutput) = findOut(out.id) match{
    case None => false;
    case Some(o) => o.close(); outs -= o; true
  }

  def addLogToHTML(id: String) = addOut(new HTMLLogOutput(id))

  def addLogToFile(fileName: String) = addOut(new FileLogOutput(fileName))
  
  def addLogToStdout() = addOut(new StdoutLogOutput())
	
  def log(str: String, logLevel: LogLevel.Value):Unit = this.synchronized {
    if(logLevel > this.logLevel) return
    val sb = StringBuilder.newBuilder
    if(showLogLevel) sb ++= logLevel.toString() + '>'
    if(logDate) sb ++= dateTag + '>'
    sb ++= str
    outs.foreach{ _.println(sb.toString) }
  }
  
  private var offset:Option[Long] = None
  
  def timer(){
    offset = Some(System.currentTimeMillis())
  }
  
  def date(){
    offset = None
  }
  
  def time = offset match {
    case Some(t) => (System.currentTimeMillis() - t)
    case None => throw new IllegalArgumentException("Logger.time called with no timer set")
  }
  
  def timerSet = offset != None
  
  private def dateTag = offset match {
    case None => val cal = Calendar.getInstance
	             StringBuilder.newBuilder
		         .append(cal.get(Calendar.YEAR)).append('-')
		         .append(cal.get(Calendar.MONTH)+1).append('-')
		         .append(cal.get(Calendar.DAY_OF_MONTH)).append('_')
		         .append(cal.get(Calendar.HOUR_OF_DAY)).append(':')
		         .append(cal.get(Calendar.MINUTE)).append(':')
		         .append(cal.get(Calendar.SECOND)).append('.')
		         .append(cal.get(Calendar.MILLISECOND)).toString
    case Some(t) => (System.currentTimeMillis() - t).toString
  }
	
  def log(expr: => Boolean, str: String, logLevel: LogLevel.Value):Unit = if(expr) log(str, logLevel)
	
  def log(source: Any, str: String, logLevel: LogLevel.Value, neuronAllowed: Boolean =false):Unit = 
    if(trackAll || neuronAllowed || isTracking(source.getClass))
	  log(source.getClass.getName + "->" + str, logLevel)
	

  def stackToString(t: Throwable) = {
	val result = new StringWriter()
	t.printStackTrace(new PrintWriter(result))
	result.toString
  }

  def exception(t: Throwable): Unit = {
	log("Throwable object caught of type " + t.getClass().getName(), LogLevel.ERROR)
	log("message: " + t.getMessage, LogLevel.ERROR)
	log("cause: " + t.getCause, LogLevel.ERROR)
	log(stackToString(t), LogLevel.ERROR)
	throw new IllegalArgumentException(t.getMessage)
  }
	
  def error(t: Throwable): Unit = {
	error("Throwable object caught of type " + t.getClass().getName())
	error("message: " + t.getMessage())
	error("cause: " + t.getCause())
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
	
  private var lastMeasure = System.currentTimeMillis

  def timer(tag: String): Long = {
	val t = System.currentTimeMillis()
	val result = t - lastMeasure
	log(tag + " took " + result + "ms.")
	lastMeasure = t
	result
  }

  def timer(source: Any, tag: String): Long = timer(source.getClass.getName + "->" + tag)

  def resetTimer(){
	lastMeasure = System.currentTimeMillis()
  }
	
}