package main.logger

import scala.collection.mutable
import main.NeuronLike

object LogLevel extends Enumeration {
  type LogLevel = Value
  val NONE, ERROR, DEBUG, INFO, COMMENT, ALL = Value
}

object LOG {
  var logLevel = LogLevel.DEBUG 
  val outs = new mutable.ListBuffer[LogOutput]()
  var logDate = true
  var showLogLevel = false

  def resetOuts() = this.synchronized {
    outs.foreach( _.close )
    outs.clear
    outs += new SystemLogOutput()
  }

  def addOut[T <: LogOutput](out: T) = this.synchronized {
    outs.find( _.id == out.id ) match {
      case Some(o) => o
      case None => outs += out; out
    }
  }
  
  private val _allowedIds = mutable.Set[String]()
  
  def log(str: String):Unit = println(str)
  
  def allow(id: String):Unit = _allowedIds += id
  def allow(ids: String*):Unit = ids.foreach( allow(_) )
  def allow(implicit n: NeuronLike):Unit = allow(n.getId)
  
  def allowedIds = _allowedIds.toSet
  def clearAllowedIds() = _allowedIds.clear()
  def removeAllowedId(id: String) = _allowedIds -= id
  
  def +=(str: String)(implicit n: NeuronLike) = log(str, n)
  def log(str: String, n: NeuronLike):Unit = if(_allowedIds.contains(n.getId)) log(str)
  
    /*
	public synchronized boolean removeOut(LogOutput out) {
		out.close();
		boolean result = false; 
		for(Iterator<LogOutput> it = outs.iterator(); it.hasNext();){
			LogOutput o = it.next();
			if(o.getId().equals(out)){
				it.remove();
				result = true;
			}
		}
		return result;
	}

	public Collection<LogOutput> getOuts() {
		return outs;
	}

	public HTMLLogOutput addLogToHTML(String id) {
		HTMLLogOutput out = new HTMLLogOutput(id);
		return addOut(out);
	}

	public FileLogOutput addLogToFile(String fileName) {
		if (fileName == null || fileName.isEmpty()) {
			return null;
		}
		FileLogOutput stream = new FileLogOutput(fileName);
		return addOut(stream);
	}
	
	private LogLevel logLevel = LogLevel.DEBUG;
	private final Collection<LogOutput> outs = new ArrayList<LogOutput>();
	private boolean logDate = true;
	private boolean showLogLevel = false;

	public boolean isShowLogLevel() {
		return showLogLevel;
	}

	public void setShowLogLevel(boolean showLogLevel) {
		this.showLogLevel = showLogLevel;
	}

	public boolean isLogDate() {
		return logDate;
	}

	public void setLogDate(boolean logDate) {
		this.logDate = logDate;
	}

	private synchronized void log(String str, LogLevel logLevel) {
		if (logLevel.ordinal() > this.logLevel.ordinal()) {
			return;
		}

		if (logDate) {
			str = str(Calendar.getInstance()) + ">" + str;
		}

		if (showLogLevel) {
			str = logLevel.name() + ">" + str;
		}

		for (LogOutput out : getOuts()) {
			out.println(str);
		}
	}

	public static void log(boolean expr, String str, LogLevel logLevel) {
		if (expr) {
			instance().log(str, logLevel);
		}
	}

	public static void log(Object source, String str, LogLevel logLevel) {
		if (instance().isTrackAll() || instance().isTracked(source) || instance().isTracked(source.getClass())) {
			instance().log(source.getClass().getName() + "->" + str, logLevel);
		}
	}

	public static String stackToString(Throwable t) {
		final Writer result = new StringWriter();
		final PrintWriter printWriter = new PrintWriter(result);
		t.printStackTrace(printWriter);
		return result.toString();
	}

	public void exception(Throwable t){
		log("Throwable object caught of type " + t.getClass().getName());
		log("message: " + t.getMessage());
		log("cause: " + t.getCause());
		log(stackToString(t));
	}
	
	public static void error(Throwable t) {
		error("Throwable object caught of type " + t.getClass().getName());
		error("message: " + t.getMessage());
		error("cause: " + t.getCause());
		error(stackToString(t));
	}

	public static void exception(String str) {
		instance().log(str, LogLevel.ERROR);
		throw new IllegalArgumentException(str);
	}
	
	public static void error(String str) {
		if (LogLevel.ERROR.ordinal() <= instance().logLevel.ordinal())
			instance().log(str, LogLevel.ERROR);
	}

	public static void debug(String str) {
		if (LogLevel.DEBUG.ordinal() <= instance().logLevel.ordinal())
			instance().log(str, LogLevel.DEBUG);
	}

	public static void info(String str) {
		if (LogLevel.INFO.ordinal() <= instance().logLevel.ordinal())
			instance().log(str, LogLevel.INFO);
	}

	public static void comment(String str) {
		if (LogLevel.COMMENT.ordinal() <= instance().logLevel.ordinal())
			instance().log(str, LogLevel.COMMENT);
	}

	public static void exception(boolean expr, String str) {
		log(expr, str, LogLevel.ERROR);
		if(expr) throw new IllegalArgumentException(str);
	}
	
	public static void error(boolean expr, String str) {
		if (LogLevel.ERROR.ordinal() <= instance().logLevel.ordinal())
			log(expr, str, LogLevel.ERROR);
	}

	public static void debug(boolean expr, String str) {
		if (LogLevel.DEBUG.ordinal() <= instance().logLevel.ordinal())
			log(expr, str, LogLevel.DEBUG);
	}

	public static void info(boolean expr, String str) {
		if (LogLevel.INFO.ordinal() <= instance().logLevel.ordinal())
			log(expr, str, LogLevel.INFO);
	}

	public static void comment(boolean expr, String str) {
		if (LogLevel.COMMENT.ordinal() <= instance().logLevel.ordinal())
			log(expr, str, LogLevel.COMMENT);
	}

	public static void exception(Object source, String str) {
		log(source, str, LogLevel.ERROR);
		throw new IllegalArgumentException(str);
	}
	
	public static void error(Object source, String str) {
		if (LogLevel.ERROR.ordinal() <= instance().logLevel.ordinal())
			log(source, str, LogLevel.ERROR);
	}

	public static void debug(Object source, String str) {
		if (LogLevel.DEBUG.ordinal() <= instance().logLevel.ordinal())
			log(source, str, LogLevel.DEBUG);
	}

	public static void info(Object source, String str) {
		if (LogLevel.INFO.ordinal() <= instance().logLevel.ordinal())
			log(source, str, LogLevel.INFO);
	}

	public static void comment(Object source, String str) {
		if (LogLevel.COMMENT.ordinal() <= instance().logLevel.ordinal())
			log(source, str, LogLevel.COMMENT);
	}

	public void log(String str) {
		log(str, logLevel);
	}

	public void log(boolean expr, String str) {
		log(expr, str, logLevel);
	}

	public void log(Object source, String str) {
		log(source, str, logLevel);
	}

	private long lastMeasure = System.currentTimeMillis();

	public static long timer(String tag) {
		long t = System.currentTimeMillis();
		long result = t - instance().lastMeasure;
		instance().log(tag + " took " + result + "ms.");
		instance().lastMeasure = t;
		return result;
	}

	public static long timer(Object source, String tag) {
		return timer(source.getClass().getName() + "->" + tag);
	}

	public static void resetTimer() {
		instance().lastMeasure = System.currentTimeMillis();
	}

	public static final String str(Calendar cal){
		return new StringBuilder()
		.append(cal.get(Calendar.YEAR)).append('-')
		.append(cal.get(Calendar.MONTH)+1).append('-')
		.append(cal.get(Calendar.DAY_OF_MONTH)).append('_')
		.append(cal.get(Calendar.HOUR_OF_DAY)).append(':')
		.append(cal.get(Calendar.MINUTE)).append(':')
		.append(cal.get(Calendar.SECOND)).append('.')
		.append(cal.get(Calendar.MILLISECOND)).toString();
	}*/
}