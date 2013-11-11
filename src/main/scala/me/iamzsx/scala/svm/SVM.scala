package me.iamzsx.scala.svm

import scala.math._
import scala.collection.mutable.ArrayBuffer

class DecisionFunction(val alpha: Array[Double], val rho: Double)

trait SVM {
  def name: String
  def trainer: SVMTrainer
}

trait SVMTrainer {
  def train(param: SVMParameter, problem: SVMProblem): SVMModel

  protected[this] def solver: FormulationSolver

  def train_one(param: SVMParameter, problem: SVMProblem, Cp: Double, Cn: Double): DecisionFunction = {
    val solution = solver.solve(problem, param)

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

class OneClassOrRegressionTrainer extends SVMTrainer {

  def train(param: SVMParameter, problem: SVMProblem): OneClassModel = {
    val nr_class = 2

    val decisionFunction = train_one(param, problem, 0, 0)

    val suportVectors = ArrayBuffer[SupportVector]()
    for (i <- 0 until problem.size if abs(decisionFunction.alpha(i)) > 0) {
      suportVectors += new SupportVector(problem.x(i), decisionFunction.alpha(i), i + 1)
    }

    new OneClassModel(
      param,
      suportVectors.toArray,
      Array(decisionFunction.rho))
  }

  def solver: FormulationSolver = new OneClassSolver
}

class OneClassTrainer extends OneClassOrRegressionTrainer {

}

class RegressionTrainer extends OneClassOrRegressionTrainer

class EpsilonSVRTrainer extends RegressionTrainer

class NuSVRTrainer extends RegressionTrainer

/**
 * <pre>
 * val param = new SVMParameter(new LinearKernel)
 * val svm = SVM("one_class")
 * val problem = SVMProblem.get(param, ...)
 * val model = svm.trainer.train(param, problem)
 * val y = model.predict(...)
 * </pre>
 *
 * @author szhu
 */
object SVM {

  def apply(name: String): SVM = name match {
    case OneClassSVM.name => OneClassSVM
    case EpsilonSVRSVM.name => EpsilonSVRSVM
    case NuSVRSVM.name => NuSVRSVM
    // TODO
    case _ => throw new IllegalArgumentException("Invalid SVM type:" + name)
  }

  def oneClass(kernel: Kernel, nu: Double, eps: Double): (SVM, SVMParameter) = {
    (SVM("one_class"), new SVMParameter(kernel))
  }
}

// val C_SVC = Value("c_svc")
// val NU_SVC = Value("nu_svc")
object OneClassSVM extends SVM {
  val name = "one_class"
  val trainer = new OneClassTrainer
}

object EpsilonSVRSVM extends SVM {
  val name = "epsilon_svr"
  val trainer = new EpsilonSVRTrainer
}

object NuSVRSVM extends SVM {
  val name = "nu_svr"
  val trainer = new NuSVRTrainer
}



//class MultipleClassSVM extends SVM {
//
//  def train(param: SVMParameter): SVMModel = {
//    val nr_class = 1;
//    null
//  }
//}