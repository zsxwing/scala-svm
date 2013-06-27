package me.iamzsx.scala.svm

object AssertUtil {

  val DELTA = 10E-6

  def svmAssertEquals(excepted: Double, actual: Double) {
    org.junit.Assert.assertEquals(excepted, actual, DELTA)
  }

  def svmAssertEquals(excepted: SupportVector, actual: SupportVector) {
    org.junit.Assert.assertEquals(excepted.vector, actual.vector)
    org.junit.Assert.assertEquals(excepted.coefficient, actual.coefficient, DELTA)
    org.junit.Assert.assertEquals(excepted.index, actual.index)
  }

  def svmAssertEquals(excepted: Seq[Double], actual: Seq[Double]) {
    org.junit.Assert.assertEquals(excepted.size, actual.size)
    (excepted zip actual) map (z => svmAssertEquals(z._1, z._2))
  }

  def svmAssertEquals(excepted: Array[SupportVector], actual: Array[SupportVector]) {
    org.junit.Assert.assertEquals(excepted.size, actual.size)
    (excepted zip actual) map (z => svmAssertEquals(z._1, z._2))
  }

  def svmAssertEquals(excepted: Array[Array[SupportVector]], actual: Array[Array[SupportVector]]) {
    org.junit.Assert.assertEquals(excepted.size, actual.size)
    (excepted zip actual) map (z => svmAssertEquals(z._1, z._2))
  }

}