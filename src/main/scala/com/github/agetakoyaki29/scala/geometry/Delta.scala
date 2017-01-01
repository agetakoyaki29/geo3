package com.github.agetakoyaki29.scala.geometry


object Delta {
  
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