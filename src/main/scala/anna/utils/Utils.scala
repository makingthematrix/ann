package anna.utils

import java.io.{IOException, FileWriter, File, BufferedWriter}
import java.text.{NumberFormat, ParsePosition}
import java.util.Locale

import akka.actor.ActorRef
import akka.pattern.ask
import anna.Context
import anna.async.{NetRef, NeuronRef}
import anna.logger.LOG

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.util.Random

import org.apache.commons.io.FileUtils

object Utils {
  def fail(str: String):IllegalArgumentException = throw new IllegalArgumentException(str)
  def assert(condition: => Boolean, str: String) = if(!condition) fail(str)
  
  def minmax(min: Double, v: Double, max: Double) = Math.max(min,Math.min(max,v))
  def minmax(min: Int, v: Int, max: Int) = Math.max(min,Math.min(max,v))
  
  /* @todo: This is weird, but it seems there has to be a sharp boundary ( <= and >= instead of < and >), 
  *  because it somehow rounds values >0.92 to 1.0 which is needed by the dotLineNet.
  *  Otherwise the signals are not strong enough.
  */
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
  
  def f(value: Double, slope: Double) = minMaxOpen(value, 0.0, 1.0, 1.0/(1.0+Math.exp(-slope*(value-0.5))) )
  def s(value: Double, slope: Double) = minmax(0.0, slope * (value - 0.5) + 0.5, 1.0)

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

  def deleteDir(filePath: String) = {
    val dir = new File(filePath)
    FileUtils.deleteDirectory(dir)
    !dir.exists()
  }

}

