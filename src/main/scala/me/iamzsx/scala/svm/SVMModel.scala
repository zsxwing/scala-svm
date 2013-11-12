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

case class SupportVector(
  val vector: List[SVMNode],
  val coefficient: Double,
  val index: Int) {
}

object SupportVector {
  def fromString(s: String): SupportVector = {
    try {
      val tokens = s.trim().split(" ")
      val vector = tokens.tail map SVMNode.fromString
      val coefficient = tokens.head.toDouble
      new SupportVector(vector.toList, coefficient, 0)
    } catch {
      case e: Throwable => throw new IOException("Invalid input: " + s, e)
    }
  }
}

trait SVMModel {

  def predict(instance: Instance): Double = predict_values(instance.x)._1

  protected[this] def predict_values(x: List[SVMNode]): (Double, Array[Double])

  def save(file: String) {} // TODO
}

class BaseModel(
  param: SVMParameter,
  supportVector: Array[SupportVector],
  rho: Array[Double]) extends SVMModel {

  override def predict_values(x: List[SVMNode]): (Double, Array[Double]) = {
    val predictResult = supportVector.map {
      supportVector => supportVector.coefficient * param.kernel(x, supportVector.vector)
    }.sum - rho(0)
    (predictResult, Array(predictResult))
  }

  override def toString = Array(
    param.toString,
    "total_sv " + supportVector.size, // TODO
    "rho " + rho.mkString(" ")).mkString("\n")
}

class OneClassModel(
  val param: SVMParameter,
  val supportVector: Array[SupportVector],
  val rho: Array[Double]) extends BaseModel(param, supportVector, rho) {

  override def predict_values(x: List[SVMNode]): (Double, Array[Double]) = {
    val (predictResult, decisionValues) = super.predict_values(x)
    if (predictResult > 0) (1, decisionValues) else (-1, decisionValues)
  }
}

class NuSVRModel(
  param: SVMParameter,
  supportVector: Array[SupportVector],
  rho: Array[Double]) extends BaseModel(param, supportVector, rho) {
}

class EpsilonSVRModel(
  param: SVMParameter,
  supportVector: Array[SupportVector],
  rho: Array[Double]) extends BaseModel(param, supportVector, rho) {
}

class SupportVectorClassificationModel(
  val nrClass: Int,
  val param: SVMParameter,
  private val supportVectors: Array[Array[SupportVector]],
  val rho: Array[Double],
  val label: Array[Int]) extends SVMModel {

  override def predict_values(x: List[SVMNode]): (Double, Array[Double]) = {
    val l = supportVectors.size;
    val kValue = supportVectors.map { supportVectorForClass =>
      supportVectorForClass.map { supportVector =>
        supportVector.coefficient * param.kernel(x, supportVector.vector)
      }
    }
    val votes = Array.fill(nrClass)(0)
    val decisionValues = Array.fill(nrClass * (nrClass - 1) / 2)(0.0)
    var p = 0
    for (
      i <- 0 until nrClass;
      j <- i + 1 until nrClass
    ) {
      val sum =
        (for (k <- 1 until supportVectors(i).size) yield supportVectors(i)(k).coefficient * kValue(i)(k)).sum +
          (for (k <- 1 until supportVectors(j).size) yield supportVectors(j)(k).coefficient * kValue(j)(k)).sum -
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

  override def toString = Array(
    param.toString,
    "total_sv " + supportVectors.size,
    "rho " + rho.mkString(" ")).mkString("\n")
}

class CSVCModel(
  nrClass: Int,
  param: SVMParameter,
  supportVectors: Array[Array[SupportVector]],
  rho: Array[Double],
  label: Array[Int]) extends SupportVectorClassificationModel(nrClass, param, supportVectors, rho, label) {
}

class NuSVCModel(
  nrClass: Int,
  param: SVMParameter,
  supportVectors: Array[Array[SupportVector]],
  rho: Array[Double],
  label: Array[Int]) extends SupportVectorClassificationModel(nrClass, param, supportVectors, rho, label) {
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


