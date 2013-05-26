package me.iamzsx.scala.svm

abstract class QMatrix {
  def getQ(i: Int, len: Int): Seq[Double]
  def getQD(): Seq[Double]
  def swapIndex(i: Int, j: Int)
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

  def Q: QMatrix

  def solve()

  def nativeSolve(p_ : Array[Double], y_ : Array[Int], alpha: Array[Int]) {
    val len = problem.size

    def getAlphaStatus(i: Int) {
      val C = if (y_(i) > 0) Cp else Cn
      if (alpha(i) >= C) UPPER_BOUND
      else if (alpha(i) < 0) LOWER_BOUND
      else FREE
    }

    val QD = Q.getQD
    val alphaStatus = Array.fill(len)(0).map(getAlphaStatus)
    val activeSet = 0 until len

    val G = p_
    val G_bar = Array.fill(len)(0)

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

