package me.iamzsx.scala.svm

import scala.math._
import org.scalatest._
import org.scalatest.matchers._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import scala.io.Source
import java.io.IOException

@RunWith(classOf[JUnitRunner])
class SVMProblemSuite extends FunSuite with BeforeAndAfter {
  val DELTA = 10E-6

  var param: SVMParameter = null

  before {
    param = new SVMParameter(SVMType.ONE_CLASS, new LinearKernel, 0, 0, false)
  }

  test("get") {
    val source = Source.fromString("-1\t1:1.000000\t2:22.080000\t3:11.460000")
    val x = Array(Array(new SVMNode(1, 1.0), new SVMNode(2, 22.08), new SVMNode(3, 11.46)))
    val y = Array(-1.0)
    val problem = SVMProblem.get(param, source)

    assertEquals(1, problem.size)
    y.zip(problem.y).foreach(y => {
      assertEquals(y._1, y._2, DELTA)
    })
    x(0).zip(problem.x(0)).foreach(x => {
      assertEquals(x._1.index, x._2.index)
      assertEquals(x._2.value, x._2.value, DELTA)
    })
    assertEquals(param.gamma, 1.0 / 3, DELTA)
  }

  test("get with multiple lines") {
    val source = Source.fromString("-1\t1:1.000000\t2:22.080000\t3:11.460000\n+1\t1:19")
    val x = Array(
      Array(new SVMNode(1, 1.0), new SVMNode(2, 22.08), new SVMNode(3, 11.46)),
      Array(new SVMNode(1, 19.0)))
    val y = Array(-1.0, 1.0)
    val problem = SVMProblem.get(param, source)

    assertEquals(2, problem.size)
    y.zip(problem.y).foreach(y => {
      assertEquals(y._1, y._2, DELTA)
    })
    x.zip(problem.x).foreach(
      x => x._1.zip(x._2).foreach(
        x => {
          assertEquals(x._1.index, x._2.index)
          assertEquals(x._2.value, x._2.value, DELTA)
        }))
    assertEquals(param.gamma, 1.0 / 3, DELTA)
  }

  test("get with param.gamma 0.1") {
    param.gamma = 0.1

    val source = Source.fromString("-1\t1:1.000000\t2:22.080000\t3:11.460000")
    val x = Array(Array(new SVMNode(1, 1.0), new SVMNode(2, 22.08), new SVMNode(3, 11.46)))
    val y = Array(-1.0)
    val problem = SVMProblem.get(param, source)

    assertEquals(param.gamma, 0.1, DELTA)
  }

  intercept[IOException] {
    val source = Source.fromString("-1\t1:1.000000\t2:22.080000\t3:11.460000\n+1")
    SVMProblem.get(param, source)
  }

  intercept[IOException] {
    val source = Source.fromString("-1\t11.000000\t2:22.080000\t3:11.460000")
    SVMProblem.get(param, source)
  }

  intercept[IOException] {
    val source = Source.fromString("-1\t4:1.000000\t2:22.080000\t3:11.460000")
    SVMProblem.get(param, source)
  }

  intercept[IOException] {
    val source = Source.fromString("-1\t2:1.000000\t2:22.080000\t3:11.460000")
    SVMProblem.get(param, source)
  }
}