package anna.data

import anna.{Context, NeuronDefaults}
import anna.async.NeuronType
import anna.logger.LOG
import anna.utils.{DoubleRange, IntRange}
import org.junit.Assert._
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

/**
 * Created by gorywoda on 27.12.14.
 */
class JsonSuite extends AnyFunSuite with BeforeAndAfter {
  before {
    LOG.addLogToStdout()
  }

  test("shouldMakeJsonFromSynapseData") {
    val expectedJson1 =
      """
        |{
        |  "neuronId":"id1",
        |  "weight":{
        |    "jsonClass":"SynapseWeight",
        |    "weight":1.0
        |  }
        |}
      """.stripMargin.trim
    val data1 = SynapseData("id1",1.0)
    assertEquals(expectedJson1, data1.toJson)

    val expectedJson2 =
      """
        |{
        |  "neuronId":"id2",
        |  "weight":{
        |    "jsonClass":"Silence"
        |  }
        |}
      """.stripMargin.trim
    val data2 = SynapseData("id2",Silence())
    assertEquals(expectedJson2, data2.toJson)
  }

  test("shouldMakeSynapseDataFromJson") {
    val json1 =
      """
        |{
        |  "neuronId":"id1",
        |  "weight":{
        |    "jsonClass":"SynapseWeight",
        |    "weight":1.0
        |  }
        |}
      """.stripMargin.trim
    val data1 = SynapseData.fromJson(json1)
    assertEquals(SynapseData("id1",1.0), data1)

    val json2 =
      """
        |{
        |  "neuronId":"id2",
        |  "weight":{
        |    "jsonClass":"Silence"
        |  }
        |}
      """.stripMargin.trim
    val data2 = SynapseData.fromJson(json2)
    assertEquals(SynapseData("id2",Silence()), data2)

    val d4 = SynapseData("id1",1.0)
    val json4 = d4.toJson
    assertEquals(d4, SynapseData.fromJson(json4))
  }

  test("shouldMakeJsonFromNeuronData") {
    val n1 = NeuronData("id1",0.0, 1, Nil)
    val json = n1.toJson
    assert(json.contains("id1"))
    assert(json.contains("0.0"))
    assert(json.contains("1"))
  }

  // ========== Context Tests ==========

  test("shouldMakeJsonFromContext") {
    val neuronDefaults = NeuronDefaults(0.5, SynapseWeight(0.1), 10, 100)
    val context = Context(5L, 1000L, neuronDefaults)
    val json = context.toJson
    assert(json.contains("awaitTimeout"))
    assert(json.contains("maxRunIterations"))
    assert(json.contains("neuronDefaults"))
  }

  test("shouldMakeJsonFromContextWithDefaults") {
    val context = Context(10L, 5000L, NeuronDefaults(0.8, SynapseWeight(0.2), 20, 200))
    val json = context.toJson
    assert(json.contains("10"))
    assert(json.contains("5000"))
    assert(json.contains("0.8"))
  }

  // ========== NeuronType Tests ==========

  test("shouldMakeJsonFromNeuronTypeStandard") {
    import NeuronType._
    val json = NeuronTypeStandard.toJson
    assert(json.nonEmpty)
  }

  test("shouldMakeJsonFromNeuronTypeDummy") {
    import NeuronType._
    val json = NeuronTypeDummy.toJson
    assert(json.nonEmpty)
  }

  test("shouldMakeJsonFromNeuronTypeSilencing") {
    import NeuronType._
    val json = NeuronTypeSilencing.toJson
    assert(json.nonEmpty)
  }

  // ========== SynapseTrait Subtypes Tests ==========

  test("shouldMakeJsonFromSynapseWeight") {
    val weight = SynapseWeight(0.75)
    val json = weight.toJson
    assert(json.contains("SynapseWeight"))
    assert(json.contains("0.75"))
  }

  test("shouldMakeJsonFromSilence") {
    val silence = Silence()
    val json = silence.toJson
    assert(json.contains("Silence"))
  }

  test("shouldMakeJsonFromWake") {
    val wake = Wake()
    val json = wake.toJson
    assert(json.nonEmpty)
  }

  // ========== NeuronData Tests with All Types ==========

  test("shouldMakeJsonFromNeuronDataWithStandardType") {
    val n1 = NeuronData("id1", 0.5, 10, Nil, NeuronType.NeuronTypeStandard)
    val json = n1.toJson
    assert(json.contains("id1"))
    assert(json.contains("0.5"))
    assert(json.contains("10"))
  }

  test("shouldMakeJsonFromNeuronDataWithDummyType") {
    val n1 = NeuronData("id2", 0.0, 5, Nil, NeuronType.NeuronTypeDummy)
    val json = n1.toJson
    assert(json.contains("id2"))
    assert(json.contains("0.0"))
  }

  test("shouldMakeJsonFromNeuronDataWithSilencingType") {
    val n1 = NeuronData("id3", 0.8, 15, Nil, NeuronType.NeuronTypeSilencing)
    val json = n1.toJson
    assert(json.contains("id3"))
    assert(json.contains("0.8"))
    assert(json.contains("15"))
  }

  test("shouldMakeJsonFromNeuronDataWithMultipleSynapses") {
    val s1 = SynapseData("target1", 0.5)
    val s2 = SynapseData("target2", Silence())
    val s3 = SynapseData("target3", Wake())
    val n1 = NeuronData("id1", 0.5, 10, List(s1, s2, s3))
    val json = n1.toJson
    assert(json.contains("target1"))
    assert(json.contains("target2"))
    assert(json.contains("target3"))
  }

  // ========== NetData Tests ==========

  test("shouldMakeJsonFromNetData") {
    val s1 = SynapseData("n2", SynapseWeight(0.5))
    val n1 = NeuronData("n1", 0.5, 10, List(s1))
    val n2 = NeuronData("n2", 0.3, 5, Nil)
    val netData = NetData("testNet", List(n1, n2), List("n1"))
    
    val json = netData.toJson
    assert(json.contains("testNet"))
    assert(json.contains("n1"))
    assert(json.contains("n2"))
  }

  // ========== DoubleRange Tests ==========

  test("shouldMakeJsonFromDoubleRange") {
    val range = DoubleRange(0.0, 1.0)
    val json = range.toJson
    assert(json.contains("from"))
    assert(json.contains("to"))
    assert(json.contains("0.0"))
    assert(json.contains("1.0"))
  }

  test("shouldMakeDoubleRangeFromJson") {
    val range = DoubleRange(0.0, 1.0)
    val json = range.toJson
    val parsedRange = DoubleRange.fromJson(json)
    assertEquals(range.from, parsedRange.from, 0.001)
    assertEquals(range.to, parsedRange.to, 0.001)
  }

  test("doubleRangeRoundTrip") {
    val range = DoubleRange(-5.5, 10.5)
    val json = range.toJson
    val parsedRange = DoubleRange.fromJson(json)
    assertEquals(range, parsedRange)
  }

  test("doubleRangeRoundTripWithEqualValues") {
    val range = DoubleRange(5.0, 5.0)
    val json = range.toJson
    val parsedRange = DoubleRange.fromJson(json)
    assertEquals(range, parsedRange)
  }

  test("doubleRangeRoundTripWithNegativeValues") {
    val range = DoubleRange(-100.0, -50.0)
    val json = range.toJson
    val parsedRange = DoubleRange.fromJson(json)
    assertEquals(range, parsedRange)
  }

  // ========== IntRange Tests ==========

  test("shouldMakeJsonFromIntRange") {
    val range = IntRange(0, 100)
    val json = range.toJson
    assert(json.contains("start"))
    assert(json.contains("end"))
    assert(json.contains("0"))
    assert(json.contains("100"))
  }

  test("shouldMakeIntRangeFromJson") {
    val range = IntRange(0, 100)
    val json = range.toJson
    val parsedRange = IntRange.fromJson(json)
    assertEquals(range.start, parsedRange.start)
    assertEquals(range.end, parsedRange.end)
  }

  test("intRangeRoundTrip") {
    val range = IntRange(-10, 20)
    val json = range.toJson
    val parsedRange = IntRange.fromJson(json)
    assertEquals(range, parsedRange)
  }

  test("intRangeRoundTripWithEqualValues") {
    val range = IntRange(42, 42)
    val json = range.toJson
    val parsedRange = IntRange.fromJson(json)
    assertEquals(range, parsedRange)
  }

  test("intRangeRoundTripWithNegativeValues") {
    val range = IntRange(-50, -10)
    val json = range.toJson
    val parsedRange = IntRange.fromJson(json)
    assertEquals(range, parsedRange)
  }

}
