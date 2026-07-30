package anna.utils

import java.io.{BufferedWriter, File, FileWriter, IOException}
import java.text.{NumberFormat, ParsePosition}
import java.util.{Calendar, Locale}
import anna.async.*
import anna.data.*
import anna.logger.LOG
import anna.logger.LOG.*
import org.apache.commons.io.FileUtils
import org.json4s.*
import org.json4s.native.Serialization

import scala.reflect.{ClassTag, Manifest}
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.Random

object Utils {
  // Helper to convert ClassTag to Manifest for json4s compatibility in Scala 3
  // This is needed because json4s still uses Manifest internally, but Scala 3
  // deprecates Manifest synthesis and prefers ClassTag
  def manifestFromClassTag[T](using ct: ClassTag[T]): Manifest[T] = {
    new Manifest[T] {
      def runtimeClass: Class[?] = ct.runtimeClass
      override def toString: String = ct.toString
    }
  }

  // Implicit conversion from ClassTag to Manifest for json4s
  implicit def classTagToManifest[T](using ct: ClassTag[T]): Manifest[T] = manifestFromClassTag(using ct)

  private def fail(str: String):IllegalArgumentException = throw new IllegalArgumentException(str)
  def assert(condition: => Boolean, str: String): Unit = if(!condition) fail(str)

  def round(x: Double, digits: Int = 2): Double = {
    val foo = math.pow(10, digits)
    math.round(x * foo) / foo
  }

  def minmax(min: Double, v: Double, max: Double): Double = Math.max(min,Math.min(max,v))
  def minmax(min: Int, v: Int, max: Int): Int = Math.max(min,Math.min(max,v))

  def parseDouble(s: String): Option[Double] = {
    val pp = new ParsePosition(0)
    val d = NumberFormat.getInstance(Locale.ENGLISH).parse(s, pp)
    if (pp.getErrorIndex == -1) Some(d.doubleValue) else None
  }

  @tailrec
  private def splitIdsRandomly(oldSet: Set[String], idsToDraw: Int, newSet: Set[String] = Set()):(Set[String],Set[String]) = idsToDraw match {
    case 0 => (newSet, oldSet)
    case n if oldSet.isEmpty => (Set(), newSet)
    case n =>
      val id = RandomNumber.apply(oldSet)
      splitIdsRandomly(oldSet - id, idsToDraw - 1, newSet + id)
  }

  private def vary(d: Double) = d + (Random.nextDouble() * 0.1 - 0.05)
  def V: Double = vary(0.95)
  def v: Double = vary(0.05)

  def save(filePath: String, body: String): Unit = {
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

  def load(filePath: String): String = {
    val source = scala.io.Source.fromFile(filePath)
    val lines = source.getLines().mkString("\n")
    source.close()
    lines
  }

  def createDir(filePath: String, deleteIfExists: Boolean =false): File = {
    val dir = new File(filePath)

    if(dir.exists() && deleteIfExists) deleteDir(filePath)

    if(!dir.exists()){
      if(!dir.mkdir()) throw new IOException(s"Failed to create a directory: $filePath")
    }

    dir
  }

  private enum ListFileMode {
    case ALL, DIRS, FILES
  }
  import ListFileMode.*

  def listDirs(dirPath: String): List[String] = list(dirPath, DIRS)
  def listFiles(dirPath: String): List[String] = list(dirPath, FILES)

  private def list(dirPath: String, mode: ListFileMode): List[String] = {
    import ListFileMode.*
    val dir = new File(dirPath)
    if(!dir.exists() || !dir.isDirectory) exception(this, s"No directory with the name $dir")

    (mode match {
      case ALL   => dir.listFiles()
      case DIRS  => dir.listFiles().filter(_.isDirectory)
      case FILES => dir.listFiles().filterNot(_.isDirectory)
    }).map(_.getName).toList
  }

  def fileExists(filePath: String): Boolean = new File(filePath).exists()

  private def deleteDir(filePath: String) = {
    val dir = new File(filePath)
    FileUtils.deleteDirectory(dir)
    !dir.exists()
  }
  

  given formats: Formats = Serialization.formats(
    ShortTypeHints(
      List(
        classOf[SynapseWeight],
        classOf[Silence],
        classOf[NeuronType]
      )
    )
  )

  private def twociphers(n: Int): String = if(n < 10) "0"+n.toString else n.toString

  def dateTag: String = {
    val cal = Calendar.getInstance
    new StringBuilder()
      .append(cal.get(Calendar.YEAR)).append('-')
      .append(twociphers(cal.get(Calendar.MONTH)+1)).append('-')
      .append(twociphers(cal.get(Calendar.DAY_OF_MONTH))).append('_')
      .append(twociphers(cal.get(Calendar.HOUR_OF_DAY))).append(':')
      .append(twociphers(cal.get(Calendar.MINUTE))).append(':')
      .append(twociphers(cal.get(Calendar.SECOND))).append('.')
      .append(cal.get(Calendar.MILLISECOND)).toString
  }

  def synapseId(fromId: String, toId: String): String = s"$fromId->$toId"
  def fromId(id: String): String = s"$id->"
  def toId(id: String): String = s"->$id"
}
