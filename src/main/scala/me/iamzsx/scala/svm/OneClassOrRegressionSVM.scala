package me.iamzsx.scala.svm

import scala.math._
import scala.collection.mutable.ArrayBuffer
import SVMType._

class DecisionFunction(val alpha: Array[Double], val rho: Double)

trait SVM {
  def train(param: SVMParameter, problem: SVMProblem): SVMModel

  def train_one(param: SVMParameter, problem: SVMProblem, Cp: Double, Cn: Double): DecisionFunction = {

    val solution =
      param.svmType match {
        case C_SVC =>
          Solver.solveOneClass(problem, param) // TODO
        case NU_SVC =>
          Solver.solveOneClass(problem, param) // TODO
        case ONE_CLASS =>
          Solver.solveOneClass(problem, param)
        case EPSILON_SVR =>
          Solver.solveOneClass(problem, param) // TODO
        case NU_SVR =>
          Solver.solveOneClass(problem, param) // TODO
      }

    println("obj = " + solution.obj + ", rho = " + solution.rho);

    val nSV = solution.alpha.count(abs(_) > 0)

    var nBSV = 0
    for (
      i <- 0 until problem.size if abs(solution.alpha(i)) > 0
    ) {
      if (problem.y(i) > 0) {
        if (abs(solution.alpha(i)) >= solution.upperBoundP)
          nBSV += 1
      } else {
        if (abs(solution.alpha(i)) >= solution.upperBoundN)
          nBSV += 1
      }
    }

    println("nSV = " + nSV + ", nBSV = " + nBSV)

    new DecisionFunction(solution.alpha, solution.rho)
  }
}

class OneClassOrRegressionSVM extends SVM {

  def train(param: SVMParameter, problem: SVMProblem): SVMModel = {
    val nr_class = 2

    val decisionFunction = train_one(param, problem, 0, 0)

    val suportVectors = ArrayBuffer[SupportVector]()
    for (i <- 0 until problem.size if abs(decisionFunction.alpha(i)) > 0) {
      suportVectors += new SupportVector(problem.x(i), decisionFunction.alpha(i), i + 1)
    }

    new SVMModel(
      nr_class,
      param,
      Array(suportVectors.toArray),
      Array(decisionFunction.rho))
  }
}

object SVM {
  def train(param: SVMParameter, problem: SVMProblem): SVMModel = {
    new OneClassOrRegressionSVM().train(param, problem)
  }
}

//class MultipleClassSVM extends SVM {
//
//  def train(param: SVMParameter): SVMModel = {
//    val nr_class = 1;
//    null
//  }
//}