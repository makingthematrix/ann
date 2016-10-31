package anna

import anna.async.{NetBuilder, NetWrapper}
import anna.data.NetData
import anna.logger.LOG._
import anna.async.NetBuilderOps._
import anna.blocks.{DelayGate, SignalSum}
import anna.logger.LOG
/**
 * Created by gorywoda on 06.06.15.
 */


object Commands {

  private var netWrapperOpt: Option[NetWrapper] = None
  private var netDataOpt: Option[NetData] = None

  val DELAY_GATE = "Delay Gate"
  val SIGNAL_SUM = "Signal Sum"
  val DOT_AND_LINE = "Dot & Line"
  val SOS = "SOS"

  def delayGate(delay: Int) = NetBuilder().netId(DELAY_GATE).addInput("IN").delayGate("DG", delay).data

  def signalSum(requiredSignals: Int) = NetBuilder().netId(SIGNAL_SUM).addInput("IN").signalSum("SS", requiredSignals).data

  def dotAndLine =
    NetBuilder().netId(DOT_AND_LINE)
      .addInput("IN").delayGate("DOT", 2)
      .use("IN").signalSum("LINE", 2)
      .use(DelayGate.outputId("DOT")).silence(SignalSum.silencingId("LINE"))
      .use(SignalSum.outputId("LINE")).silence(DelayGate.silencingId("DOT"))
      .data

  def sos = {
    val dotBlockName = "DOT"
    val dotExpectedDelay = 2
    val dotOutputId = DelayGate.outputId(dotBlockName)
    val dotSilencingId = DelayGate.silencingId(dotBlockName)
    val dotInputId = DelayGate.inputId(dotBlockName)
    val dotMiddleId = DelayGate.middleId(dotBlockName)

    val lineBlockName = "LINE"
    val lineRequiredSignals = 2
    val lineOutputId = SignalSum.outputId(lineBlockName)
    val lineSilencingId = SignalSum.silencingId(lineBlockName)
    val lineInputId = SignalSum.inputId(lineBlockName)

    val sBlockName = "S"
    val sRequiredSignals = 3
    val sOutputId = SignalSum.outputId(sBlockName)
    val sSilencingId = SignalSum.silencingId(sBlockName)
    val sInputId = SignalSum.inputId(sBlockName)

    val oBlockName = "O"
    val oRequiredSignals = 3
    val oOutputId = SignalSum.outputId(oBlockName)
    val oSilencingId = SignalSum.silencingId(oBlockName)
    val oInputId = SignalSum.inputId(oBlockName)

    NetBuilder().netId(SOS)
      .addInput("IN").delayGate(dotBlockName, dotExpectedDelay)
      .use("IN").signalSum(lineBlockName, lineRequiredSignals)
      .use(dotOutputId).silence(lineSilencingId)
      .use(lineOutputId).silence(dotSilencingId)
      .use(dotOutputId).signalSum(sBlockName, sRequiredSignals)
      .use(lineOutputId).signalSum(oBlockName, oRequiredSignals)
      .use(sOutputId).silence(oSilencingId)
      .use(oOutputId).silence(sSilencingId)
      .data
  }

  private def wrapper = netWrapperOpt.getOrElse(throw new IllegalArgumentException("NetWrapper not set"))

  private def addAfterFireTrigger(neuronId: String)(f: => Any) = wrapper.addAfterFire(neuronId)(f)
  private def addSilenceRequestedTrigger(neuronId: String)(f: => Any) = wrapper.addSilenceRequested(neuronId)(f)
  private def addSignalIgnoredTrigger(neuronId: String)(f: => Any) = wrapper.addSignalIgnored(neuronId)(f)

  val outputBuffer = StringBuilder.newBuilder
  def output = outputBuffer.toString
  def clearOutput() = outputBuffer.clear()

  private def initializeNetwork(netData: NetData) = {
    LOG.timer()
    clearOutput()
    netDataOpt = Some(netData)
    netWrapperOpt = Some(NetBuilder().set(netData).build())
    LOG.clearAllowedIds()
  }

  private def setupDelayGate(netData: NetData) = {
    initializeNetwork(netData)

    addAfterFireTrigger(DelayGate.outputId("DG")){
      LOG.debug("Pushing '1' into the output")
      outputBuffer.append('1')
    }

    LOG.allow(
      "IN",
      DelayGate.inputId("DG"),
      DelayGate.silencingId("DG"),
      DelayGate.middleId("DG"),
      DelayGate.outputId("DG")
    )
  }

  private def setupSignalSum(netData: NetData) = {
    initializeNetwork(netData)

    addAfterFireTrigger(SignalSum.outputId("SS")){
      LOG.debug("Pushing '1' into the output")
      outputBuffer.append('1')
    }

    LOG.allow(
      "IN",
      SignalSum.inputId("SS"),
      SignalSum.silencingId("SS"),
      SignalSum.outputId("SS")
    )
  }

  private def setupDotAndLine(netData: NetData) = {
    initializeNetwork(netData)

    val dotInputId = DelayGate.inputId("DOT")
    val dotMiddleId = DelayGate.middleId("DOT")
    val dotSilencingId = DelayGate.silencingId("DOT")
    val dotOutputId = DelayGate.outputId("DOT")

    addAfterFireTrigger(dotOutputId){
      LOG.debug("Pushing '.' into the output")
      outputBuffer.append('.')
    }

    val lineInputId = SignalSum.inputId("LINE")
    val lineSilencingId = SignalSum.silencingId("LINE")
    val lineOutputId = SignalSum.outputId("LINE")

    addAfterFireTrigger(lineOutputId){
      LOG.debug("Pushing '-' into the output")
      outputBuffer.append('-')
    }

    LOG.allow(
      "IN",
      dotInputId, dotMiddleId, dotOutputId, dotSilencingId,
      lineInputId, lineOutputId, lineSilencingId
    )
  }

  private def setupSOS(netData: NetData) = {
    initializeNetwork(netData)

    val dotInputId = DelayGate.inputId("DOT")
    val dotMiddleId = DelayGate.middleId("DOT")
    val dotSilencingId = DelayGate.silencingId("DOT")
    val dotOutputId = DelayGate.outputId("DOT")

    val lineInputId = SignalSum.inputId("LINE")
    val lineSilencingId = SignalSum.silencingId("LINE")
    val lineOutputId = SignalSum.outputId("LINE")

    val sInputId = SignalSum.inputId("S")
    val sSilencingId = SignalSum.silencingId("S")
    val sOutputId = SignalSum.outputId("S")

    addAfterFireTrigger(sOutputId){
      LOG.debug("Pushing 'S' into the output")
      outputBuffer.append('S')
    }

    val oInputId = SignalSum.inputId("O")
    val oSilencingId = SignalSum.silencingId("O")
    val oOutputId = SignalSum.outputId("O")

    addAfterFireTrigger(oOutputId){
      LOG.debug("Pushing 'O' into the output")
      outputBuffer.append('O')
    }

    LOG.allow(
      "IN",
      dotInputId, dotMiddleId, dotOutputId, dotSilencingId,
      lineInputId, lineOutputId, lineSilencingId,
      sInputId, sOutputId, sSilencingId,
      oInputId, oOutputId, oSilencingId
    )
  }

  def setup(netData: NetData) = netData.id match {
    case DELAY_GATE => setupDelayGate(netData)
    case SIGNAL_SUM => setupSignalSum(netData)
    case DOT_AND_LINE => setupDotAndLine(netData)
    case SOS => setupSOS(netData)
    case other => error(s"netId not recognized: $other")
  }

  def send(sequence: String) = {
    val validSequence = sequence.replaceAll(",","").toCharArray.mkString(",") // just to make sure
    wrapper.reset()
    wrapper.resetIterations()
    LOG.timer()
    LOG.startLoggingIterations(() => wrapper.iteration)
    wrapper.iterateUntilCalm(validSequence)
    Thread.sleep(Context().iterationTime)
  }

  def print(netData: NetData):Unit = {
    LOG.debug(netData.toJson)
  }

  def print: Unit = netDataOpt match {
    case Some(netData) => print(netData)
    case None =>
  }


}
