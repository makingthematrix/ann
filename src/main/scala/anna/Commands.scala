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

  val DELAY_GATE = "Delay Gate"
  val SIGNAL_SUM = "Signal Sum"
  val DOT_AND_LINE = "Dot & Line"
  val SOS = "SOS"

  def delayGate(delay: Int) = NetBuilder().netId(DELAY_GATE).addInput("IN").delayGate("DG", delay).data

  def signalSum(requiredSignals: Int) = NetBuilder().netId(SIGNAL_SUM).addInput("IN").signalSum("SS", requiredSignals).data

  lazy val dotAndLine =
    NetBuilder().netId(DOT_AND_LINE)
      .addInput("IN").delayGate("DOT", 2)
      .use("IN").signalSum("LINE", 2)
      .use(DelayGate.outputId("DOT")).silence(SignalSum.silencingId("LINE"))
      .use(SignalSum.outputId("LINE")).silence(DelayGate.silencingId("DOT"))
      .data

  lazy val sos = {
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

  def initializeNetwork(netData: NetData) = {
    LOG.timer()
    clearOutput()
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

  def setup(netData: NetData) = {
    shutdown()
    netData.id match {
      case DELAY_GATE => setupDelayGate(netData)
      case SIGNAL_SUM => setupSignalSum(netData)
      case DOT_AND_LINE => setupDotAndLine(netData)
      case SOS => setupSOS(netData)
      case other => error(s"netId not recognized: $other")
    }
  }

  private def resetBeforeWork(sequence: String) = {
    wrapper.reset()
    wrapper.resetIterations()
    clearOutput()
    LOG.timer()
    LOG.startLoggingIterations(() => wrapper.iteration)
    sequence.replaceAll(",","").toCharArray.mkString(",") // just to make sure
  }

  def send(sequence: String):Unit = {
    val validSequence = resetBeforeWork(sequence)
    wrapper.iterateUntilCalm(validSequence)
    Thread.sleep(Context().iterationTime)
  }

  def send(sequence: String, repeatTimes: Int):Unit = {
    val validSequence = resetBeforeWork(sequence)
    for(i <- 1 to repeatTimes){
      wrapper.iterateUntilCalm(validSequence)
    }
    Thread.sleep(Context().iterationTime)
  }

  def shutdown() = netWrapperOpt match {
    case Some(netWrapper) => netWrapper.shutdown()
    case _ =>
  }

  def neuronsIds = wrapper.neuronIds
  def maxIterations = wrapper.maxIterations
  def setMaxIterations(maxIterations: Int) = {
    wrapper.maxIterations = maxIterations
  }

  def print(netData: NetData):Unit = LOG.debug(netData.toJson)

  def help = println(
    s"""
      | You can use pre-constructed networks:
      | 1. sos
      | 2. dotAndLine
      | and plus utility methods for network construction:
      | 3. delayGate(delay: Int)
      | 4. signalSum(requestedSum: Int)
      | All four give you an object of the type NetData which you can then pass to the setup method:
      | > setup(sos)
      | > setup(dotAndLine)
      | > setup(delayGate(2))
      | > setup(signalSum(3))
      | This will build the network and display the list of neuron ids.
      |
      | The setup method works only for these four networks. If you want to build one of your own, please look into
      | the code to learn how to use NetBuilder and then call initializeNetwork(netData: NetData). Please note that
      | the setup method adds triggers to output neurons (different for each network). If you build a network by hand,
      | you have to do this by hand as well.
      |
      | The output neurons for the networks are, respectively:
      | 1. sos: S2 (sends 'S' to the output) and O2 (sends 'O')
      | 2. dotAndLine: DOT3 (sends '.') and LINE2 (sends '-')
      | 3. delayGate(_): DG3 (sends '1')
      | 4. signalSum(_): SS2 (sends '1')
      |
      | You can also print the JSON form of each network, eg.:
      | > print(delayGate(2))
      |
      | After the setup, you can send input vectors to the network and see how it handles them. Since all four networks
      | have only one input neuron, you can use a utility method and send the whole input vectors' sequence in form
      | of a string, eg.:
      | > send("100010001000110011001100100010001000")
      | The string will be parsed into signals which will be sent to the input neuron and from there to other neurons
      | in the network. Then you can see the output by typing:
      | > output
      |
      | If you want to send longer input sequences, please modify the maximum number of iterations:
      | > setMaxIterations(_)
      | The variable was introduced in order to prevent infinite processing which is possible with customized networks.
      | The default number is $maxIterations. You can check it by typing:
      | > maxIterations
      |
      | During the processing the console will display logs from the neurons, about what signals they received and sent,
      | their internal state, etc. You can control logs from which neurons you want to see, by typing:
      | > LOG.allow(neuronId: String)
      | > LOG.removedAllowedId(neuronId: String)
      | > LOG.clearAllowedIds()
      | Do that between the setup and the send methods. The next setup will reset the list.
    """.stripMargin)

}
