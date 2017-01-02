package com.github.agetakoyaki29.scala.geometry


object Delta {

  implicit val delta: Double = 1 / Math.pow(2, 50)

  def ipsilon(d: Double): Double = NotNaN orElse NotInfinite orElse AllDouble andThen { _ / Math.pow(2, 53) } andThen { Math.abs(_) } apply d

  def eq0(d: Double, deltas: Double*): Boolean = Math.abs(d) < deltas.max
  def eq(d1: Double, d2: Double)(implicit delta: Double): Boolean = eq0(d1-d2, delta, ipsilon(d1), ipsilon(d2))

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
