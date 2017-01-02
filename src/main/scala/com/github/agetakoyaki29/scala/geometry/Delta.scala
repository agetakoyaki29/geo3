package com.github.agetakoyaki29.scala.geometry


object Delta {

  val EXPONENT_SIZE = 11
  val FRACTION_SIZE = 52
  val FRACTION_PRECISION = 53

  implicit val delta: Double = 1 / Math.pow(2, 50)

  def ipsilon(d: Double): Double = NotNaN orElse NotInfinite orElse AllDouble andThen
    { _ / Math.pow(2, FRACTION_PRECISION) } andThen { Math.abs(_) } apply d

  def eq(d1: Double, d2: Double): Boolean = eq0(d1-d2, ipsilon(d1), ipsilon(d2))
  def eq0(d: Double, deltas: Double*)(implicit delta: Double): Boolean = Math.abs(d) < (deltas :+ delta).max

  def gt(d1: Double, d2: Double): Boolean = gt0(d1-d2, ipsilon(d1), ipsilon(d2))
  def gt0(d: Double, deltas: Double*)(implicit delta: Double): Boolean = d >= -(deltas :+ delta).max

  def lt(d1: Double, d2: Double): Boolean = lt0(d1-d2, ipsilon(d1), ipsilon(d2))
  def lt0(d: Double, deltas: Double*)(implicit delta: Double): Boolean = d <= (deltas :+ delta).max

  // ---- for validation ----

  val NotNaN: PartialFunction[Double, Double] = {
    case d if d.isNaN => throw new IllegalArgumentException("Not NaN")
  }
  val NotInfinite: PartialFunction[Double, Double] = {
    case d if d.isInfinite => throw new IllegalArgumentException("Not Infinite")
  }
  val NotZero: PartialFunction[Double, Double] = {
    case d if d == 0 => throw new IllegalArgumentException("Not Zero")
  }
  val AllDouble: PartialFunction[Double, Double] = { case d => d }

}
