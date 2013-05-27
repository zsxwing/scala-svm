package me.iamzsx.scala.svm

abstract class QMatrix {
  def getQ(i: Int, len: Int): Seq[Double]
  def getQD(): Seq[Double]
  def swapIndex(i: Int, j: Int)

  def apply(i: Int, len: Int) = 1.0
}

class OneClassQMatrix(val problem: SVMProblem, val param: SVMParameter) extends QMatrix {
  val x: Array[List[SVMNode]] = Array(List(new SVMNode(1, 0.2)))
  val qd = (0 until problem.x.size).map(i => param.kernel(x(i), x(i)))

  override def getQ(i: Int, len: Int) = {
    (0 until len).map(j => param.kernel(x(i), x(j)))
  }

  def getQD(): Seq[Double] = qd

  def swapIndex(i: Int, j: Int) {

  }
}

abstract class Solver(
  val problem: SVMProblem,
  val param: SVMParameter,
  val Cp: Double,
  val Cn: Double,
  val epsilon: Double,
  val shrinking: Boolean) {

  val LOWER_BOUND = 2
  val UPPER_BOUND = 1
  val FREE = 3

  val TAU = 1e-12

  def Q: QMatrix

  def solve()

  val len = problem.size

  val y = Array(1)

  val alpha = Array(0.1)

  def getAlphaStatus(i: Int) = {
    val C = if (y(i) > 0) Cp else Cn
    if (alpha(i) >= C) UPPER_BOUND
    else if (alpha(i) < 0) LOWER_BOUND
    else FREE
  }

  def isUpperBound(i: Int) = getAlphaStatus(i) == UPPER_BOUND
  def isLowerBound(i: Int) = getAlphaStatus(i) == LOWER_BOUND

  def nativeSolve(p_ : Array[Double], y_ : Array[Int], alpha: Array[Int]) {
    val len = problem.size

    val QD = Q.getQD
    val alphaStatus = Array.fill(len)(0).map(getAlphaStatus)
    val activeSet = 0 until len

    val G = p_
    val G_bar = Array.fill(len)(0)
    var iter = 0
    val maxIter = 10000000 max (if (len > Int.MaxValue / 100) Int.MaxValue else len / 100)
    while (iter < maxIter) {
      iter = iter + 1

    }
    val (i, j) = selectWorkingSet
    if (j == -1) {
      val (i, j) = selectWorkingSet
      if (j == -1) {
        // TODO
      }
    }
  }

  val C = 0.3

  val G = Array(0.1)

  def selectWorkingSet = {
    var maxG = Double.MinValue
    var i = -1

    for (t <- 0 until len) {
      if (y(t) == +1) {
        if (!isUpperBound(t) && -G(t) >= maxG) {
          i = t
          maxG = -G(t)
        }
      } else {
        if (!isLowerBound(t) && G(t) >= maxG) {
          i = t
          maxG = G(t)
        }
      }
    }

    var j = -1
    var minObj = Double.MaxValue
    var minG = Double.MaxValue

    for (t <- 0 until len) {
      if ((y(i) == +1 && !isLowerBound(t)) ||
        (y(i) == -1 && !isUpperBound(t))) {
        val gradDiff = maxG + y(t) * G(t)
        if (-y(t) * G(t) <= minG) {
          minG = -y(t) * G(t)
        }
        if (gradDiff > 0) {
          val quadCoef = Q(i, i) + Q(t, t) - 2 * y(i) * y(t) * Q(i, t)
          val objDiff = if (quadCoef <= 0) {
            -(gradDiff * gradDiff) / TAU
          } else {
            -(gradDiff * gradDiff) / quadCoef
          }
          if (objDiff <= minObj) {
            j = t
            minObj = objDiff
          }
        }
      }
    }

    if (maxG - minG < epsilon) {
      (-1, -1)
    }
    (i, j)
  }
}

class OneClassSolver(problem: SVMProblem,
  param: SVMParameter,
  Cp: Double,
  Cn: Double,
  epsilon: Double,
  shrinking: Boolean) extends Solver(problem, param, Cp, Cn, epsilon, shrinking) {

  override val Q = new OneClassQMatrix(problem, param)

  override def solve() {
    val n = (param.nu * problem.size).toInt
    val alpha = (0 until problem.size).map(_ match {
      case i if i < n => 1
      case i if i == n && i < problem.size => n
      case _ => 0
    }).toArray

    val zeros = Array.fill(problem.size)(0.0)
    val ones = Array.fill(problem.size)(1)

    nativeSolve(zeros, ones, alpha)

  }
}

class Solution {

}

