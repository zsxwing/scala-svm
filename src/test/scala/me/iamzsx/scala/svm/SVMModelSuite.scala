package me.iamzsx.scala.svm

import scala.math._
import org.scalatest._
import org.scalatest.matchers._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import scala.io.Source
import java.io.IOException
import AssertUtil._
import Coefficients._

@RunWith(classOf[JUnitRunner])
class SVMModelSuite extends FunSuite with BeforeAndAfter {
  val DELTA = 10E-6

  test("OneClassModel") {
    // ./svm-train -s 2 -t 0 -r 0.2 test.data test.model
    val param = new SVMParameter(LinearKernel, 0, 0)
    val supportVector = Array(
      SupportVector.fromString("1:1 2:22.08 3:11.46"),
      SupportVector.fromString("1:19"))
    val coefficientVector = Array(0.3627269429755065, 0.6372730570244935)
    val model = new OneClassModel(param, supportVector, coefficientVector, 236.947)

    assertPredictInstance("+1 1:1.000000 2:22.080000 3:11.460000", model)
    assertPredictInstance("+1 1:19", model)
  }

  test("NuSVRModel") {
    // ./svm-train -s 4 -t 0 -r 0.2 test.data test.model
    val param = new SVMParameter(LinearKernel, 0, 0)
    val supportVector = Array(
      SupportVector.fromString("1:1 2:22.08 3:11.46"),
      SupportVector.fromString("1:19"))
    val coefficientVector = Array(-0.002121210192839229, 0.002121210192839219)
    val model = new NuSVRModel(param, supportVector, coefficientVector, -0.274546)

    assertPredictInstance("-1 1:1.000000 2:22.080000 3:11.460000", model)
    assertPredictInstance("+1 1:19", model)
  }

  test("EpsilonSVRModel") {
    // ./svm-train -s 3 -t 0 -r 0.2 test.data test.model
    val param = new SVMParameter(LinearKernel, 0, 0)
    val supportVector = Array(
      SupportVector.fromString("1:19"),
      SupportVector.fromString("1:1 2:22.08 3:11.46"))
    val coefficientVector = Array(0.002121210192839219, -0.002121210192839219)
    val model = new EpsilonSVRModel(param, supportVector, coefficientVector, -0.274546)

    assertPredictInstance("-1 1:1.000000 2:22.080000 3:11.460000", model)
    assertPredictInstance("+1 1:19", model)
  }

  test("CSVCModel") {
    // ./svm-train -s 0 -t 0 -r 0.2 test.data test.model
    val param = new SVMParameter(LinearKernel, 0, 0)
    val supportVector = Array(
      Array(
        SupportVector.fromString("1:1 2:22.08 3:11.46")),
      Array(
        SupportVector.fromString("1:19")),
      Array(
        SupportVector.fromString("2:34 3:15")),
      Array(
        SupportVector.fromString("1:10 3:22")))
    val coefficients = new Coefficients(
      Array(
        Array(
          null,
          (Array(0.002121210192839219), Array(-0.002121210192839219)),
          (Array(0.01285198370368465), Array(-0.01285198370368465)),
          (Array(0.002942829648420142), Array(-0.002942829648420142))),
        Array(
          null,
          null,
          (Array(0.001148105625717566), Array(-0.001148105625717566)),
          (Array(0.003539823008849557), Array(-0.003539823008849557))),
        Array(
          null,
          null,
          null,
          (Array(0.001532567049808429), Array(-0.001532567049808429)))))
    val rho = Array(0.274546, -4.89109, 0.052762, -0.585534, -0.39469, 0.610728)
    val label = 1 to 4
    val model = new CSVCModel(4, param, supportVector, coefficients, rho, Array(), Array(), label)
    assertPredictInstance("1 1:1.000000 2:22.080000 3:11.460000", model)
    assertPredictInstance("2 1:19", model)
    assertPredictInstance("3 2:34 3:15", model)
    assertPredictInstance("4 1:10 3:22", model)
  }

  test("NuSVCModel") {
    // ./svm-train -s 1 -t 0 -r 0.2 test.data test.model
    val param = new SVMParameter(LinearKernel, 0, 0)
    val supportVector = Array(
      Array(
        SupportVector.fromString("2:13.08 3:7")),
      Array(
        SupportVector.fromString("1:19")),
      Array(
        SupportVector.fromString("2:34 3:15"), SupportVector.fromString("2:-1")),
      Array(
        SupportVector.fromString("1:10 3:22")))
    val coefficients = new Coefficients(
      Array(
        Array(
          null,
          (Array(0.003441828960374912), Array(-0.002121210192839219)),
          (Array(1.934584364555769), Array(-0.7975824366423717, -1.137001927913397)),
          (Array(0.004031555793506938), Array(-0.004031555793506938))),
        Array(
          null,
          null,
          (Array(0.005537785840454479), Array(-0.0001336706927006254, -0.005404115147753854)),
          (Array(0.003539823008849557), Array(-0.003539823008849557))),
        Array(
          null,
          null,
          null,
          (Array(0.001020943323660012, 0.003034858921016748), Array(-0.00405580224467676)))))
    val rho = Array(-0.2425, 1.67644, -0.733569, 0.999141, -0.39469, -1.03168)
    val label = 1 to 4
    val model = new NuSVCModel(4, param, supportVector, coefficients, rho, Array(), Array(), label)

    assertPredictInstance("1 1:1.000000 2:22.080000 3:11.460000", model)
    assertPredictInstance("1 2:13.080000 3:7.0000", model)
    assertPredictInstance("2 1:19", model)
    assertPredictInstance("3 2:34 3:15", model)
    assertPredictInstance("3 2:-1", model)
    assertPredictInstance("4 1:10 3:22", model)
  }

  def assertPredictInstance(testCase: String, model: SVMModel): Unit = {
    val instance = Instance.fromString(testCase)
    val label = model.predict(instance)
    assertEquals(instance.y, label, DELTA)
  }
}