package me.iamzsx.scala.svm

import java.io._
import scala.math._
import scala.io._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

trait Gamma {
  def gamma: Double
}

trait Degree {
  def degree: Double
}

trait Coef0 {
  def coef0: Double
}

class PolyParameter(
  override val gamma: Double,
  override val degree: Double,
  override val coef0: Double) extends Gamma with Degree with Coef0

class SigmoidParameter(
  override val gamma: Double,
  override val coef0: Double) extends Gamma with Coef0

class SVMParameter(
  val kernel: Kernel,
  val nu: Double = 0.5,
  val eps: Double = 0.001,
  var gamma: Double = 0.0) {

  override def toString = Array(
    kernel.toString).mkString("\n")
}

class SVMTrainParameter {

}

object SVMParameter {
  def poly(gamma: Double, degree: Double, coef0: Double) = new PolyParameter(gamma, degree, coef0)
}

class EpsilonSVRSVMParamter(
  kernel: Kernel,
  nu: Double,
  eps: Double,
  val C: Double,
  val p: Double) extends SVMParameter(kernel, nu, eps) {
}

import Coefficients._

case class Coefficients(
  val coefficients: Array[Array[ClassifierCoefficient]]) {

  def get(classifier: Classifier): ClassifierCoefficient = {
    coefficients(classifier._1)(classifier._2)
  }
}

object Coefficients {
  type Classifier = (Int, Int)
  type CoefficientVector = Array[Double]
  type ClassifierCoefficient = (CoefficientVector, CoefficientVector)
}

case class SupportVector(
  val vector: List[SVMNode],
  val index: Int) {
}

object SupportVector {
  def fromString(s: String): SupportVector = {
    try {
      val tokens = s.trim().split(" ")
      val vector = tokens map SVMNode.fromString
      new SupportVector(vector.toList, 0)
    } catch {
      case e: Throwable => throw new IOException("Invalid input: " + s, e)
    }
  }
}

trait SVMModel {

  def predict(instance: Instance): Double = predictValues(instance.x)._1

  def predictProbability(instance: Instance, prob_estimates: Array[Double]): Double = predict(instance)

  protected[this] def predictValues(x: List[SVMNode]): (Double, Array[Double])

  def save(file: String) {} // TODO
}

import Coefficients._

class BaseModel(
  param: SVMParameter,
  supportVector: Array[SupportVector],
  coefficientVector: CoefficientVector,
  rho: Double) extends SVMModel {

  require(supportVector.size == coefficientVector.size)

  override def predictValues(x: List[SVMNode]): (Double, Array[Double]) = {
    val predictResult = ((0 until supportVector.size) map {
      i => coefficientVector(i) * param.kernel(x, supportVector(i).vector)
    }).sum - rho
    (predictResult, Array(predictResult))
  }

  override def toString = Array(
    param.toString,
    "total_sv " + supportVector.size, // TODO
    "rho " + rho).mkString("\n")
}

class OneClassModel(
  val param: SVMParameter,
  val supportVector: Array[SupportVector],
  val coefficientVector: CoefficientVector,
  val rho: Double) extends BaseModel(param, supportVector, coefficientVector, rho) {

  override def predictValues(x: List[SVMNode]): (Double, Array[Double]) = {
    val (predictResult, decisionValues) = super.predictValues(x)
    if (predictResult > 0) (1, decisionValues) else (-1, decisionValues)
  }
}

class NuSVRModel(
  param: SVMParameter,
  supportVector: Array[SupportVector],
  coefficientVector: CoefficientVector,
  rho: Double) extends BaseModel(param, supportVector, coefficientVector, rho) {
}

class EpsilonSVRModel(
  param: SVMParameter,
  supportVector: Array[SupportVector],
  coefficientVector: CoefficientVector,
  rho: Double) extends BaseModel(param, supportVector, coefficientVector, rho) {
}

class SupportVectorClassificationModel(
  val nrClass: Int,
  val param: SVMParameter,
  val supportVectors: Array[Array[SupportVector]],
  val coefficients: Coefficients,
  val rho: Array[Double],
  val probA: Array[Double],
  val probB: Array[Double],
  val label: Seq[Int]) extends SVMModel {

  // TODO
  require(supportVectors.size * (supportVectors.size - 1) / 2 == rho.size)

  override def predictValues(x: List[SVMNode]): (Double, Array[Double]) = {
    val l = supportVectors.size;
    val kValue = supportVectors.map { supportVectorForClass =>
      supportVectorForClass.map { supportVector =>
        param.kernel(x, supportVector.vector)
      }
    }
    val votes = Array.fill(nrClass)(0)
    val decisionValues = Array.fill(nrClass * (nrClass - 1) / 2)(0.0)
    var p = 0
    for (
      i <- 0 until nrClass;
      j <- i + 1 until nrClass
    ) {
      val (coefficientVectorOfI, coefficientVectorOfJ) = coefficients.get(i, j)
      val sum =
        (for (k <- 0 until supportVectors(i).size) yield coefficientVectorOfI(k) * kValue(i)(k)).sum +
          (for (k <- 0 until supportVectors(j).size) yield coefficientVectorOfJ(k) * kValue(j)(k)).sum -
          rho(p)
      if (sum > 0) {
        votes(i) = votes(i) + 1
      } else {
        votes(j) = votes(j) + 1
      }
      decisionValues(p) = sum
      p += 1
    }
    (label(votes.maxBy(i => votes(i))), decisionValues)
  }

  override def predictProbability(instance: Instance, prob_estimates: Array[Double]): Double =
    if (probA.length != 0 && probB.length != 0) {
      val (predictResult, decisionValues) = predictValues(instance.x)
      val pairwise_prob = Array.ofDim[Double](nrClass, nrClass)
      val min_prob = 1e-7
      var k = 0
      for (
        i <- 0 until nrClass;
        j <- i + 1 until nrClass
      ) {
        pairwise_prob(i)(j) = sigmoid_predict(decisionValues(k), probA(k), probB(k)) max min_prob min (1 - min_prob)
        pairwise_prob(j)(i) = 1 - pairwise_prob(i)(j)
        k += 1
      }

      multiclass_probability(nrClass, pairwise_prob, prob_estimates)

      var prob_max_idx = 0
      for (i <- 1 until nrClass) {
        if (prob_estimates(i) > prob_estimates(prob_max_idx))
          prob_max_idx = i;
      }
      label(prob_max_idx)
    } else {
      predict(instance)
    }

  // http://www.csie.ntu.edu.tw/~cjlin/papers/svmprob/svmprob.pdf
  private[this] def multiclass_probability(k: Int, r: Array[Array[Double]], p: Array[Double]): Unit = {
    assert(k != 0)
    val max_iter = 100 max k
    val eps = 0.005 / k
    val Q = Array.ofDim[Double](k, k)
    val Qp = Array.ofDim[Double](k)

    for (t <- 0 until k) {
      p(t) = 1.0 / k
      Q(t)(t) = 0
      for (j <- 0 until t) {
        Q(t)(t) += r(j)(t) * r(j)(t)
        Q(t)(j) = Q(j)(t)
      }
      for (j <- t + 1 until k) {
        Q(t)(t) += r(j)(t) * r(j)(t)
        Q(t)(j) = -r(j)(t) * r(t)(j)
      }
    }

    var pQp = 0.0
    import scala.util.control.Breaks._

    var iter = 0
    breakable {
      while (iter < max_iter) {
        pQp = 0.0

        for (t <- 0 until k) {
          Qp(t) = 0
          for (j <- 0 until k) {
            Qp(t) += Q(t)(j) * p(j)
          }
          pQp += p(t) * Qp(t)
        }
        val maxError = Qp.map(t => (t - pQp) abs) max;
        if (maxError < eps) {
          break
        }
        for (t <- 0 until k) {
          val diff = (-Qp(t) + pQp) / Q(t)(t)
          p(t) += diff
          pQp = (pQp + diff * (diff * Q(t)(t) + 2 * Qp(t))) / (1 + diff) / (1 + diff);
          for (j <- 0 until k) {
            Qp(j) = (Qp(j) + diff * Q(t)(j)) / (1 + diff);
            p(j) /= (1 + diff);
          }
        }
        iter += 1
      }
    }

    if (iter >= max_iter) {
      println("Exceeds max_iter in multiclass_prob");
    }
  }

  private[this] def sigmoid_predict(decision_value: Double, A: Double, B: Double): Double =
    {
      val fApB = decision_value * A + B;
      // 1-p used later; avoid catastrophic cancellation
      if (fApB >= 0)
        exp(-fApB) / (1.0 + exp(-fApB));
      else
        1.0 / (1 + exp(fApB));
    }

  override def toString = Array(
    param.toString,
    "total_sv " + supportVectors.size,
    "rho " + rho.mkString(" ")).mkString("\n")
}

class CSVCModel(
  nrClass: Int,
  param: SVMParameter,
  supportVectors: Array[Array[SupportVector]],
  coefficients: Coefficients,
  rho: Array[Double],
  probA: Array[Double],
  probB: Array[Double],
  label: Seq[Int]) extends SupportVectorClassificationModel(nrClass, param, supportVectors, coefficients, rho, probA, probB, label) {
}

class NuSVCModel(
  nrClass: Int,
  param: SVMParameter,
  supportVectors: Array[Array[SupportVector]],
  coefficients: Coefficients,
  rho: Array[Double],
  probA: Array[Double],
  probB: Array[Double],
  label: Seq[Int]) extends SupportVectorClassificationModel(nrClass, param, supportVectors, coefficients, rho, probA, probB, label) {
}

object SVMModel {

  def load(file: String): SVMModel = {
    for (line <- Source.fromFile(file).getLines) {
      val splits = line.split(" ")
      splits(0) match {
        case "svm_type" => ""
        case "kernel_type" => ""
        case "degree" => ""
        case "gamma" => ""
        case "coef0" => ""
        case "nr_class" => ""
        case "total_sv" => ""
        case "rho" => ""
        case "label" => ""
        case "probA" => ""
        case "probB" => ""
        case "nr_sv" => ""
        case "SV" => ""
      }
    }
    null
  }

  def save(file: String) {
    val output = new PrintWriter(new FileWriter(file))

    output.close()
    //val output = Resource.fromFile(file)(Codec.UTF8)
  }
}

class SolutionInfo {
  val upper_bound_p: Double = 0;
  val upper_bound_n: Double = 0;
  val rho: Double = 0;
}


