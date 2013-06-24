package me.iamzsx.scala.svm

import java.io.IOException

import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.abs

import SVMType._

class SVMProblem(val param: SVMParameter, val y: Array[Double], val x: Array[List[SVMNode]]) {
  require(y.size == x.size)

  val size = y.size

  def train_one(Cp: Double, Cn: Double) = {
    val alpha = Array(0.0)

    param.svmType match {
      case C_SVC =>
        solve_c_svc
      case NU_SVC =>
        solve_nu_svc
      case ONE_CLASS =>
        solve_one_class
      case EPSILON_SVR =>
        solve_epsilon_svr
      case NU_SVR =>
        solve_nu_svr
    }

    val si = new SolutionInfo
    val nSV = alpha.filter(abs(_) > 0).size
    val nBSV =
      0 until alpha.size filter (i => abs(alpha(i)) > 0 &&
        (y(i) > 0 && abs(alpha(i)) >= si.upper_bound_p) ||
        (y(i) <= 0 && abs(alpha(i)) >= si.upper_bound_n)) size

    new DecisionFunction(alpha, si.rho)
  }

  def train {

  }

  def solve_c_svc {

  }

  def solve_nu_svc {

  }

  def solve_one_class {

  }

  def solve_epsilon_svr {

  }

  def solve_nu_svr {

  }

}

object SVMProblem {

  def get(param: SVMParameter, source: Source) = {
    val y = ArrayBuffer[Double]()
    val x = ArrayBuffer[List[SVMNode]]()
    var maxIndex = 0
    for (line <- source.getLines.map(_.trim)) {
      val splits = line.split('\t')
      if (splits.size <= 1) {
        // Do we need to support no feature?
        throw new IOException("Invalid input: " + line)
      }
      val (Array(label), features) = splits.splitAt(1)
      y += label.toDouble
      var featureMaxIndex = 0
      x += features.map((feature: String) => {
        val splits = feature.split(':')
        if (splits.size != 2) {
          throw new IOException("Invalid input: " + line)
        }
        val index = splits(0).toInt
        if (index <= featureMaxIndex) {
          throw new IOException("Index must be in order and unique: " + line)
        } else {
          featureMaxIndex = index
        }
        val value = splits(1).toDouble
        SVMNode(index, value)
      }).toList
      maxIndex = maxIndex max featureMaxIndex
    }
    if (param.gamma == 0 && maxIndex > 0) {
      param.gamma = 1.0 / maxIndex
    }
    new SVMProblem(param, y.toArray, x.toArray)
  }

}
