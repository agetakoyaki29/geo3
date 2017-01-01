package com.github.agetakoyaki29.scala.geometry.dim2

import com.github.agetakoyaki29.scala.geometry.Delta._
import scala.reflect.ClassTag


abstract class Dim2Factory[T <: Dim2 : ClassTag] {
  def apply(x: Double, y: Double): T
  def apply(op: Dim2): T = op match {
    case t: T => t
    case dim2 => clone(dim2)
  }
  def clone(dim2: Dim2): T = apply(dim2.x, dim2.y)
}


object Dim2 extends Dim2Factory[Dim2] {
  class SimpleDim2 private[Dim2] (val x: Double, val y: Double) extends Dim2
  
  def apply(x: Double, y: Double): Dim2 = new SimpleDim2(x, y)
}


abstract class Dim2 {
  def x: Double
  def y: Double

  // -- validate --

  validate

  protected def validate = {
    validateElement(x)
    validateElement(y)
  }

  protected def validateElement(d: Double): Unit = NotNaN orElse NotMinValue orElse AllDouble apply d

  // ----

  def factory: Dim2Factory[_ <: Dim2] = Dim2

  def isZero: Boolean = x==0 && y==0
  def isInfinite = x.isInfinite || y.isInfinite

  def mapD2(f: Double => Double): Dim2 = factory(f(x), f(y))
  def zipmap(op: Dim2)(f: (Double, Double) => Double): Dim2 = factory(f(x, op.x), f(y, op.y))

  // -- IndexedSeq --

  def foreach[U](f: Double => U): Unit = { f(x); f(y) }
  def apply(idx: Int): Double = idx match { case 0 => x; case 1 => y }
  def length: Int = 2
}
