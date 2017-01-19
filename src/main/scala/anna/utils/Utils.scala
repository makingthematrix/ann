package anna.utils

import java.io.{BufferedWriter, File, FileWriter, IOException}
import java.text.{NumberFormat, ParsePosition}
import java.util.{Calendar, Locale}

import akka.actor.ActorRef
import akka.pattern.ask
import anna.Context
import anna.async._
import anna.data._
import anna.logger.LOG
import anna.logger.LOG._
import org.apache.commons.io.FileUtils
import org.json4s._
import org.json4s.native.Serialization

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.util.Random

object Utils {
  def fail(str: String):IllegalArgumentException = throw new IllegalArgumentException(str)
  def assert(condition: => Boolean, str: String) = if(!condition) fail(str)

  def round(x: Double, digits: Int = 2) = {
    val foo = math.pow(10, digits)
    math.round(x * foo) / foo
  }

  def minmax(min: Double, v: Double, max: Double) = Math.max(min,Math.min(max,v))
  def minmax(min: Int, v: Int, max: Int) = Math.max(min,Math.min(max,v))

  def minMaxClosed(x: Double, min: Double, max: Double, d: => Double) = x match {
    case _ if x <= min => min
    case _ if x >= max => max
    case _ => d
  }
  
  def minMaxOpen(x: Double, min: Double, max: Double, d: => Double) = x match {
    case _ if x < min => min
    case _ if x > max => max
    case _ => d
  }

  implicit val timeout = Context().timeout

  def await[T](ref: ActorRef, msg: Any): T = Await.result(ref ? msg, timeout.duration).asInstanceOf[T]
  def await[T](net: NetRef, msg: Any): T = await[T](net.ref, msg)
  def await[T](neuron: NeuronRef, msg: Any): T = await[T](neuron.ref, msg)

  def parseDouble(s: String) = {
    val pp = new ParsePosition(0)
    val d = NumberFormat.getInstance(Locale.ENGLISH).parse(s, pp)
    if (pp.getErrorIndex == -1) Some(d.doubleValue) else None
  }

  @tailrec
  def splitIdsRandomly(oldSet: Set[String], idsToDraw: Int, newSet: Set[String] = Set()):(Set[String],Set[String]) = idsToDraw match {
    case 0 => (newSet, oldSet)
    case n if oldSet.isEmpty => (Set(), newSet)
    case n =>
      val id = RandomNumber.apply(oldSet)
      splitIdsRandomly(oldSet.toSet - id, idsToDraw - 1, newSet + id)
  }

  def vary(d: Double) = d + (Random.nextDouble() * 0.1 - 0.05)
  def V = vary(0.95)
  def v = vary(0.05)

  def save(filePath: String, body: String) = {
    var writer:BufferedWriter = null // I blame Java
    try {
      val file = new File(filePath)
      if(file.exists()) file.delete()

      file.createNewFile()
      file.setReadable(true)
      file.setWritable(true)

      writer = new BufferedWriter(new FileWriter(file, true))
      writer.write(body)
      writer.newLine()
      writer.flush()
    } catch {
      case ex: IOException => LOG.exception(ex)
    } finally {
      if(writer != null) writer.close()
    }

  }

  def load(filePath: String) = {
    val source = scala.io.Source.fromFile(filePath)
    val lines = source.getLines().mkString("\n")
    source.close()
    lines
  }

  def createDir(filePath: String, deleteIfExists: Boolean =false) = {
    val dir = new File(filePath)

    if(dir.exists() && deleteIfExists) deleteDir(filePath)

    if(!dir.exists()){
      if(!dir.mkdir()) throw new IOException(s"Failed to create a directory: $filePath")
    }

    dir
  }

  def listDirs(dirPath: String) = list(dirPath, 1)
  def listFiles(dirPath: String) = list(dirPath, 2)

  private def list(dirPath: String, flag: Int) = {
    val dir = new File(dirPath)
    if(!dir.exists() || !dir.isDirectory) exception(this, s"No directory with the name $dir")

    (flag match {
      case 0 => dir.listFiles()
      case 1 => dir.listFiles().filter(_.isDirectory)
      case 2 => dir.listFiles().filterNot(_.isDirectory)
    }).map(_.getName).toList
  }

  def fileExists(filePath: String) = new File(filePath).exists()

  def deleteDir(filePath: String) = {
    val dir = new File(filePath)
    FileUtils.deleteDirectory(dir)
    !dir.exists()
  }

  implicit val formats = Serialization.formats(
    ShortTypeHints(
      List(
        classOf[SynapseWeight],
        classOf[Silence],
        classOf[SpeakUp],
        classOf[NeuronTypeStandard],
        classOf[NeuronTypeDummy],
        classOf[NeuronTypeSilencing],
        classOf[SilenceIterations],
        classOf[SilenceForever]
      )
    )
  )

  private def twociphers(n: Int) = if(n < 10) "0"+n.toString else n.toString

  def dateTag = {
    val cal = Calendar.getInstance
    StringBuilder.newBuilder
      .append(cal.get(Calendar.YEAR)).append('-')
      .append(twociphers(cal.get(Calendar.MONTH)+1)).append('-')
      .append(twociphers(cal.get(Calendar.DAY_OF_MONTH))).append('_')
      .append(twociphers(cal.get(Calendar.HOUR_OF_DAY))).append(':')
      .append(twociphers(cal.get(Calendar.MINUTE))).append(':')
      .append(twociphers(cal.get(Calendar.SECOND))).append('.')
      .append(cal.get(Calendar.MILLISECOND)).toString
  }

  def synapseId(fromId: String, toId: String) = s"$fromId->$toId"
  def fromId(id: String) = s"$id->"
  def toId(id: String) = s"->$id"

}

