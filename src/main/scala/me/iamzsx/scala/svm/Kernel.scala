package me.iamzsx.scala.svm

import java.io._

import scala.math._
import scala.io._

object KernelType extends Enumeration {
  type KernelType = Value
  val LINEAR = Value("linear")
  val POLY = Value("poly")
  val RBF = Value("rbf")
  val SIGMOID = Value("sigmoid")
  val PRECOMPUTED = Value

  def dot(x: List[SVMNode], y: List[SVMNode]): Double = {
    if (x == Nil || y == Nil) 0
    else {
      if (x.head.index == y.head.index) {
        x.head.value * y.head.value + dot(x.tail, y.tail)
      } else {
        if (x.head.index < y.head.index) dot(x.tail, y)
        else dot(x, y.tail)
      }
    }
  }

  def powi(base: Double, times: Int) = {
    assert(times >= 0)
    var tmp = base
    var ret = 1.0
    var t = times
    while (t > 0) {
      if ((t & 1) == 1) ret *= tmp;
      tmp *= tmp;
      t >>= 1;
    }
    ret
  }

}

import KernelType._
/**
 * @see http://en.wikipedia.org/wiki/Support_vector_machine
 */
trait Kernel {
  def kernelType: KernelType
  def apply(x: List[SVMNode], y: List[SVMNode]): Double
}

/**
 * @see http://en.wikipedia.org/wiki/Support_vector_machine
 */
class LinearKernel extends Kernel {

  override def kernelType = LINEAR

  override def apply(x: List[SVMNode], y: List[SVMNode]) = {
    dot(x, y)
  }

  override def toString = "kernel_type " + kernelType.toString
}

/**
 *
 */
class PolynomialKernel(val gamma: Double, val coef0: Double = 0, val degree: Int = 3) extends Kernel {
  require(degree >= 0) // Why degree == 0 is valid?

  override def kernelType = POLY
  override def apply(x: List[SVMNode], y: List[SVMNode]) = {
    powi(gamma * dot(x, y) + coef0, degree);
  }
  override def toString = Array(
    "kernel_type " + kernelType.toString,
    "degree " + degree,
    "gamma " + gamma,
    "coef0 " + coef0).mkString("\n")
}

/**
 * @see http://en.wikipedia.org/wiki/Support_vector_machine#Nonlinear_classification
 * @see http://en.wikipedia.org/RBF_kernel
 */
class RBFKernel(val gamma: Double) extends Kernel {
  require(gamma >= 0) // Why gamma == 0 is valid?

  override def kernelType = RBF

  override def apply(x: List[SVMNode], y: List[SVMNode]) = {
    def rbf(x: List[SVMNode], y: List[SVMNode], sum: Double): Double = {
      if (x == Nil && y == Nil) exp(-gamma * sum);
      else if (x == Nil)
        rbf(Nil, y.tail, sum + y.head.value * y.head.value);
      else if (y == Nil)
        rbf(x.tail, Nil, sum + x.head.value * x.head.value);
      else {
        if (x.head.index == y.head.index) {
          rbf(x.tail, y.tail, (x.head.value - y.head.value) * (x.head.value - y.head.value) + sum)
        } else {
          if (x.head.index < y.head.index) rbf(x.tail, y, sum + x.head.value * x.head.value)
          else rbf(x, y.tail, sum + y.head.value * y.head.value)
        }
      }
    }
    rbf(x, y, 0)
  }

  override def toString = Array(
    "kernel_type " + kernelType.toString,
    "gamma " + gamma).mkString("\n")
}

/**
 * @see http://en.wikipedia.org/wiki/Support_vector_machine#Nonlinear_classification
 */
class SigmoidKernel(val gamma: Double, val coef0: Double = 0) extends Kernel {
  require(gamma >= 0) // Why gamma == 0 is valid?

  override def kernelType = SIGMOID

  override def apply(x: List[SVMNode], y: List[SVMNode]) = {
    tanh(gamma * dot(x, y) + coef0);
  }

  override def toString = Array(
    "kernel_type " + kernelType.toString,
    "gamma " + gamma,
    "coef0 " + coef0).mkString("\n")
}

class PrecomputedKernel extends Kernel {

  override def kernelType = PRECOMPUTED

  override def apply(x: List[SVMNode], y: List[SVMNode]) = {
    // TODO
    0
  }

  override def toString = "kernel_type " + kernelType.toString

}