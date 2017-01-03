package com.github.agetakoyaki29.scala.geometry.dim2

import scala.reflect.ClassTag

import com.github.agetakoyaki29.scala.sameret.UpRet
import com.github.agetakoyaki29.scala.geometry.Delta
import Delta._


abstract class Dim2Factory[T <: Dim2 : ClassTag] {
  def apply(x: Double, y: Double): T
  def apply(op: Dim2): T = op match {
    case t: T => t
    case dim2 => clone(dim2)
  }
  def clone(dim2: Dim2): T = apply(dim2.x, dim2.y)
}


object Dim2 extends Dim2Factory[Dim2] {
  class SimpleDim2 private[Dim2] (x: Double, y: Double) extends Dim2(x, y)

  def apply(x: Double, y: Double): Dim2 = new SimpleDim2(x, y)

  val ZERO = this(0, 0)
  val INFINITY = this(Double.PositiveInfinity, Double.PositiveInfinity)
}


abstract class Dim2(val x: Double, val y: Double) {

  // -- validate --

  validate

  protected def validate = {
    validateElement(x)
    validateElement(y)
  }

  protected def validateElement(d: Double): Unit = NotNaN orElse AllDouble apply d

  // ----

  def factory: Dim2Factory[_ <: Dim2] = Dim2

  final def isZero: Boolean = x==0 && y==0
  final def isInfinite = x.isInfinite || y.isInfinite

	def same(op: Dim2): Boolean = Delta.eq(x, op.x) && Delta.eq(y, op.y)

  @UpRet
  def mapD2(f: Double => Double): Dim2 = factory(f(x), f(y))
  @UpRet
  def zipmapD2(op: Dim2)(f: (Double, Double) => Double): Dim2 = factory(f(x, op.x), f(y, op.y))

  def reduceLeft[T](f: (Double, Double) => T): T = f(x, y)

  @UpRet
  def updated(x: Double, y: Double): Dim2 = factory(x, y)
  @UpRet
  def updatedX(x: Double): Dim2 = factory(x, y)
  @UpRet
  def updatedY(y: Double): Dim2 = factory(x, y)

  // -- std --

  override def toString = this.getClass.getSimpleName + s"(${x}, ${y})"

  override def equals(op: Any) = op match {
    case dim2: Dim2 => x==dim2.x && y==dim2.y
    case _ => false
  }

  override def hashCode = x.## * 31 + y.##  // TODO Provisional

  // -- IndexedSeq --

  def foreach[U](f: Double => U): Unit = { f(x); f(y) }
  def apply(idx: Int): Double = idx match { case 0 => x; case 1 => y }
  def length: Int = 2

}
