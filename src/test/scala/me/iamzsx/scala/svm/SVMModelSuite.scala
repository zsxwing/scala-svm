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

@RunWith(classOf[JUnitRunner])
class SVMModelSuite extends FunSuite with BeforeAndAfter {
  val DELTA = 10E-6

  test("OneClassModel") {
    val param = new SVMParameter(LinearKernel, 0, 0)
    val supportVector = Array(
      SupportVector.fromString("0.002121210192839219 1:19"),
      SupportVector.fromString("-0.002121210192839219 1:1 2:22.08 3:11.46"))
    val model = new OneClassModel(param, supportVector, Array(-0.274546))

    val instance1 = Instance.fromString("-1 1:1.000000 2:22.080000 3:11.460000")
    val label1 = model.predict(instance1)
    assertEquals(instance1.y, label1, DELTA)

    val instance2 = Instance.fromString("+1 1:19")
    val label2 = model.predict(instance2)
    assertEquals(instance2.y, label2, DELTA)
  }

  test("NuSVRModel") {
    val param = new SVMParameter(LinearKernel, 0, 0)
    val supportVector = Array(
      SupportVector.fromString("0.002121210192839219 1:19"),
      SupportVector.fromString("-0.002121210192839219 1:1 2:22.08 3:11.46"))
    val model = new NuSVRModel(param, supportVector, Array(-0.274546))

    val instance1 = Instance.fromString("-1 1:1.000000 2:22.080000 3:11.460000")
    val label1 = model.predict(instance1)
    assertEquals(instance1.y, label1, DELTA)

    val instance2 = Instance.fromString("+1 1:19")
    val label2 = model.predict(instance2)
    assertEquals(instance2.y, label2, DELTA)
  }

  test("EpsilonSVRModel") {
    val param = new SVMParameter(LinearKernel, 0, 0)
    val supportVector = Array(
      SupportVector.fromString("0.002121210192839219 1:19"),
      SupportVector.fromString("-0.002121210192839219 1:1 2:22.08 3:11.46"))
    val model = new EpsilonSVRModel(param, supportVector, Array(-0.274546))

    val instance1 = Instance.fromString("-1 1:1.000000 2:22.080000 3:11.460000")
    val label1 = model.predict(instance1)
    assertEquals(instance1.y, label1, DELTA)

    val instance2 = Instance.fromString("+1 1:19")
    val label2 = model.predict(instance2)
    assertEquals(instance2.y, label2, DELTA)
  }
}