package me.iamzsx.scala.svm

import scala.math._

import org.scalatest._
import org.scalatest.matchers._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._

import KernelType._
import AssertUtil._

@RunWith(classOf[JUnitRunner])
class KernelTypeSuite extends FunSuite {

  test("dot") {
    val x = List(new SVMNode(1, 0.4), new SVMNode(2, 0.5))
    val y = List(new SVMNode(1, 0.9), new SVMNode(3, 0.4))
    svmAssertEquals(0.36, dot(x, y))
  }

  test("dot with normal") {
    val x = List(new SVMNode(1, 0.4), new SVMNode(2, 0.5))
    val y = List(new SVMNode(1, 0.9), new SVMNode(2, 0.4))
    svmAssertEquals(0.56, dot(x, y))
  }

  test("dot with empty") {
    val x = List(new SVMNode(1, 0.4), new SVMNode(2, 0.5))
    val y = List(new SVMNode(4, 0.9), new SVMNode(3, 0.4))
    svmAssertEquals(0, dot(x, y))
  }

  test("powi") {
    svmAssertEquals(16, powi(2.0, 4))
    svmAssertEquals(1, powi(2.0, 0))
    svmAssertEquals(1.21, powi(1.1, 2))
    svmAssertEquals(1.23 * 1.23 * 1.23 * 1.23 * 1.23, powi(1.23, 5))
  }
}

@RunWith(classOf[JUnitRunner])
class LinearKernelSuite extends FunSuite {

  val DELTA = 10E-6

  test("toString") {
    assertEquals("kernel_type linear", new LinearKernel().toString)
  }

}

@RunWith(classOf[JUnitRunner])
class PolynomialKernelSuite extends FunSuite {

  val DELTA = 10E-6

  test("toString") {
    assertEquals("kernel_type poly\n" +
      "degree 3\n" +
      "gamma 1.0\n" +
      "coef0 2.0", new PolynomialKernel(1.0, 2.0, 3).toString)
  }

  test("apply") {
    val x = List(new SVMNode(1, 0.4), new SVMNode(2, 0.5))
    val y = List(new SVMNode(1, 0.9), new SVMNode(3, 0.4))

    svmAssertEquals(pow(1.0 * 0.36 + 2.0, 3), new PolynomialKernel(1.0, 2.0, 3)(x, y))
  }
}

@RunWith(classOf[JUnitRunner])
class RBFKernelSuite extends FunSuite {

  test("toString") {
    assertEquals("kernel_type rbf\n" +
      "gamma 1.0", new RBFKernel(1.0).toString)
  }

  test("apply") {
    val x = List(new SVMNode(1, 0.4), new SVMNode(2, 0.5))
    val y = List(new SVMNode(1, 0.9), new SVMNode(3, 0.4))

    svmAssertEquals(exp(-2.0 * 0.66), new RBFKernel(2.0)(x, y))
  }
}

@RunWith(classOf[JUnitRunner])
class SigmoidKernelSuite extends FunSuite {

  test("toString") {
    assertEquals("kernel_type sigmoid\n" +
      "gamma 1.0\n" +
      "coef0 2.0", new SigmoidKernel(1.0, 2.0).toString)
  }

  test("apply") {
    val x = List(new SVMNode(1, 0.4), new SVMNode(2, 0.5))
    val y = List(new SVMNode(1, 0.9), new SVMNode(3, 0.4))

    svmAssertEquals(tanh(1.0 * 0.36 + 2.0), new SigmoidKernel(1.0, 2.0)(x, y))
  }
}
