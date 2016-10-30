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
    val dotHushId = DelayGate.silencingId(dotBlockName)
    val dotInputId = DelayGate.inputId(dotBlockName)
    val dotMiddleId = DelayGate.middleId(dotBlockName)

    val lineBlockName = "LINE"
    val lineRequiredSignals = 2
    val lineOutputId = SignalSum.outputId(lineBlockName)
    val lineHushId = SignalSum.silencingId(lineBlockName)
    val lineInputId = SignalSum.inputId(lineBlockName)

    val sBlockName = "S"
    val sRequiredSignals = 3
    val sOutputId = SignalSum.outputId(sBlockName)
    val sHushId = SignalSum.silencingId(sBlockName)
    val sInputId = SignalSum.inputId(sBlockName)

    val oBlockName = "O"
    val oRequiredSignals = 3
    val oOutputId = SignalSum.outputId(oBlockName)
    val oHushId = SignalSum.silencingId(oBlockName)
    val oInputId = SignalSum.inputId(oBlockName)

    NetBuilder().netId(SOS)
      .addInput("IN").delayGate(dotBlockName, dotExpectedDelay)
      .use("IN").signalSum(lineBlockName, lineRequiredSignals)
      .use(dotOutputId).silence(lineHushId)
      .use(lineOutputId).silence(dotHushId)
      .use(dotOutputId).signalSum(sBlockName, sRequiredSignals)
      .use(lineOutputId).signalSum(oBlockName, oRequiredSignals)
      .use(sOutputId).silence(oHushId)
      .use(oOutputId).silence(sHushId)
      .data
  }

  private def wrapper = netWrapperOpt.getOrElse(throw new IllegalArgumentException("NetWrapper not set"))

  private def setupTriggers(inputId: String, silencingId: String, outputId: String) = {
    LOG.debug("trigger setup")
    wrapper.addAfterFire(outputId)( ()=>{
      LOG.debug(s"iteration: ${wrapper.iteration}, $outputId fired")
    })

    wrapper.addSilenceRequested(silencingId)( ()=>{
      LOG.debug(s"iteration: ${wrapper.iteration}, $silencingId silence requested")
    })

    wrapper.addSignalIgnored(inputId)( ()=>{
      LOG.debug(s"iteration: ${wrapper.iteration}, $inputId signal ignored")
    })
  }

  private def setupDelayGate(netData: NetData) = {
    LOG.timer()
    netDataOpt = Some(netData)
    netWrapperOpt = Some(NetBuilder().set(netData).build())

    setupTriggers(
      DelayGate.inputId("DG"),
      DelayGate.silencingId("DG"),
      DelayGate.outputId("DG")
    )

    LOG.clearAllowedIds()
    LOG.allow(
      "IN",
      DelayGate.inputId("DG"),
      DelayGate.silencingId("DG"),
      DelayGate.middleId("DG"),
      DelayGate.outputId("DG")
    )
  }

  private def setupSignalSum(netData: NetData) = {
    LOG.timer()
    netDataOpt = Some(netData)
    netWrapperOpt = Some(NetBuilder().set(netData).build())

    setupTriggers(
      SignalSum.inputId("SS"),
      SignalSum.silencingId("SS"),
      SignalSum.outputId("SS")
    )

    LOG.clearAllowedIds()
    LOG.allow(
      "IN",
      SignalSum.inputId("SS"),
      SignalSum.silencingId("SS"),
      SignalSum.outputId("SS")
    )
  }

  private def setupDotAndLine(netData: NetData) = {
    LOG.timer()
    netDataOpt = Some(netData)
    netWrapperOpt = Some(NetBuilder().set(netData).build())

    val dotInputId = DelayGate.inputId("DOT")
    val dotMiddleId = DelayGate.middleId("DOT")
    val dotSilencingId = DelayGate.silencingId("DOT")
    val dotOutputId = DelayGate.outputId("DOT")

    setupTriggers(dotInputId, dotSilencingId, dotOutputId)

    val lineInputId = SignalSum.inputId("LINE")
    val lineSilencingId = SignalSum.silencingId("LINE")
    val lineOutputId = SignalSum.outputId("LINE")

    setupTriggers(lineInputId, lineSilencingId, lineOutputId)

    LOG.clearAllowedIds()
    LOG.allow(
      "IN",
      dotInputId, dotMiddleId, dotOutputId, dotSilencingId,
      lineInputId, lineOutputId, lineSilencingId
    )
  }

  private def setupSOS(netData: NetData) = {
    LOG.timer()
    netDataOpt = Some(netData)
    netWrapperOpt = Some(NetBuilder().set(netData).build())

    val dotInputId = DelayGate.inputId("DOT")
    val dotMiddleId = DelayGate.middleId("DOT")
    val dotSilencingId = DelayGate.silencingId("DOT")
    val dotOutputId = DelayGate.outputId("DOT")

    setupTriggers(dotInputId, dotSilencingId, dotOutputId)

    val lineInputId = SignalSum.inputId("LINE")
    val lineSilencingId = SignalSum.silencingId("LINE")
    val lineOutputId = SignalSum.outputId("LINE")

    setupTriggers(lineInputId, lineSilencingId, lineOutputId)

    val sInputId = SignalSum.inputId("S")
    val sSilencingId = SignalSum.silencingId("S")
    val sOutputId = SignalSum.outputId("S")

    setupTriggers(sInputId, sSilencingId, sOutputId)

    val oInputId = SignalSum.inputId("O")
    val oSilencingId = SignalSum.silencingId("O")
    val oOutputId = SignalSum.outputId("O")

    setupTriggers(oInputId, oSilencingId, oOutputId)

    LOG.clearAllowedIds()
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
