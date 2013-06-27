package me.iamzsx.scala.svm

import java.io._
import scala.math._
import scala.io._

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object SVMType extends Enumeration {
  type SVMType = Value
  val C_SVC = Value("c_svc")
  val NU_SVC = Value("nu_svc")
  val ONE_CLASS = Value("one_class");
  val EPSILON_SVR = Value("epsilon_svr");
  val NU_SVR = Value("nu_svr");
}

import SVMType._

class SVMNode(
  val index: Int,
  val value: Double) {

  override def toString = (index, value).toString
}

object SVMNode {
  def apply(index: Int, value: Double) = new SVMNode(index, value)
}

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
  val svmType: SVMType,
  val kernel: Kernel,
  val nu: Double = 0.5,
  val eps: Double = 0.001,
  val shrinking: Boolean = false) {

  var gamma = 0.0

  override def toString = Array(
    "svm_type " + svmType, kernel.toString).mkString("\n")
}

class SVMTrainParameter {

}

object SVMParameter {
  def poly(gamma: Double, degree: Double, coef0: Double) = new PolyParameter(gamma, degree, coef0)
}

class EpsilonSVRSVMParamter(
  svmType: SVMType,
  kernel: Kernel,
  nu: Double,
  eps: Double,
  shrinking: Boolean,
  val C: Double,
  val p: Double) extends SVMParameter(svmType, kernel, nu, eps, shrinking) {

}

case class SupportVector(
  val vector: List[SVMNode],
  val coefficient: Double,
  val index: Int) {

//  override def toString = vector.toString + " " + coefficient + " " + index
}

class SVMModel(
  val nr_class: Int,
  val param: SVMParameter,
  val supportVectors: Array[Array[SupportVector]],
  val rho: Array[Double]) {

  require(supportVectors.size == rho.size)

  def predict(x: List[SVMNode]) = {
    predict_values(x)
  }

  def predict_values(x: List[SVMNode]): (Double) = 0.0

  override def toString = Array(
    param.toString,
    "total_sv " + supportVectors.size,
    "rho " + rho.mkString(" ")).mkString("\n")
}

class BaseModel(
  param: SVMParameter,
  supportVectors: Array[Array[SupportVector]],
  rho: Array[Double]) extends SVMModel(2, param, supportVectors, rho) {

  override def predict_values(x: List[SVMNode]): Double = {
    supportVectors(0).map(supportVector => param.kernel(x, supportVector.vector)).sum - rho(0);
  }
}

class CSVCModel {

}

class NUSVCModel {

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


