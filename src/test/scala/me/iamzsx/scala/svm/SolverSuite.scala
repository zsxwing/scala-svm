package me.iamzsx.scala.svm
import scala.math._
import org.scalatest._
import org.scalatest.matchers._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import KernelType._
import SVMType._
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class SolverSuite extends FunSuite {

  val DELTA = 10E-6

  test("1 train case") {
    val param = new SVMParameter(
      SVMType.ONE_CLASS,
      new LinearKernel)

    val source = Source.fromString("-1\t1:1.0\t2:22.08\t3:11.46")
    val problem = SVMProblem.get(param, source)

    val solution = Solver.solveOneClass(problem, param)
    assertEquals(77.482250, solution.obj, DELTA)
    assertEquals(309.929000, solution.rho, DELTA)
    assertEquals(1, solution.alpha.size)
    assertEquals(0.5, solution.alpha(0), DELTA)
    assertEquals(1, solution.upperBoundP, DELTA)
    assertEquals(1, solution.upperBoundN, DELTA)
    assertEquals(0, solution.r, DELTA)
  }

  test("2 train case") {
    val param = new SVMParameter(
      SVMType.ONE_CLASS,
      new LinearKernel)

    val source = Source.fromString("-1\t1:1.0\t2:22.08\t3:11.46\n+1\t1:2.0\t2:22.08\t3:11.46")
    val problem = SVMProblem.get(param, source)

    val solution = Solver.solveOneClass(problem, param)
    assertEquals(309.929000, solution.obj, DELTA)
    assertEquals(620.358000, solution.rho, DELTA)
    assertEquals(2, solution.alpha.size)
    assertEquals(1, solution.alpha(0), DELTA)
    assertEquals(0, solution.alpha(1), DELTA)
    assertEquals(1, solution.upperBoundP, DELTA)
    assertEquals(1, solution.upperBoundN, DELTA)
    assertEquals(0, solution.r, DELTA)
  }
}