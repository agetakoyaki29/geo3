package com.github.agetakoyaki29.scala.geometry.dim2.point

import scala.reflect.ClassTag

import com.github.agetakoyaki29.scala.sameret.SameRet
import com.github.agetakoyaki29.scala.sameret.UpRet
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2Factory
import com.github.agetakoyaki29.scala.geometry.dim2.Vector


object Point extends Dim2Factory[Point] {
  def apply(x: Double, y: Double) = new Point(x, y)

  val ORIGIN = this(Dim2.ZERO)
}


@SameRet
class Point protected (x: Double, y: Double) extends Vector(x, y) with Trans[Point] {

  override def factory: Dim2Factory[_ <: Point] = Point

  def distance(op: Point) = (this localize op).norm
  def distanceSqr(op: Point) = (this localize op).normSqr

  // ---- use trans ----

  // def localize[T](trans: Trans[T]): T = trans localizedBy this
  def localize[A: ClassTag](any: A): A = any match {
    case trans: Trans[A] => trans localizedBy this
    case any => any
  }
  // def localize[A: ClassTag](seq: Seq[A]): Seq[A] = seq.map{localize[A](_)}

  // def unlocalize[T](trans: Trans[T]): T = trans unlocalizedBy this
  def unlocalize[A: ClassTag](any: A): A = any match {
    case trans: Trans[A] => trans unlocalizedBy this
    case any => any
  }
  // def unlocalize[A: ClassTag](seq: Seq[A]): Seq[A] = seq.map{unlocalize[A](_)}

  def fff[T: ClassTag, R: ClassTag](f: T => R): T => R = f compose localize[T] andThen unlocalize[R]
  // def fff[T: ClassTag, R: ClassTag](f: Seq[T] => R): Seq[T] => R = f compose localize[T] andThen unlocalize[R]
  // def fff[T, R](f: T => R): T => R = f match {
  //   case f: Trans[TT] => Trans[TR] =>
  // }

  // ---- for trans ----

  def +(op: Point) = this.+(Vector(op))
  def -(op: Point) = this.-(Vector(op))

  // ---- UpRet ----

  override def mapD2(f: Double => Double) = factory(super.mapD2(f))
  override def zipmapD2(op: Dim2)(f: (Double, Double) => Double) = factory(super.zipmapD2(op)(f))
  override def updated(x: Double, y: Double) = factory(super.updated(x, y))
  override def updatedX(x: Double) = factory(super.updatedX(x))
  override def updatedY(y: Double) = factory(super.updatedY(y))

  override def abs = factory(super.abs)
  override def unary_+() = factory(super.unary_+)
  override def unary_-() = factory(super.unary_-)
  override def +(op: Vector) = factory(super.+(op))
  override def -(op: Vector) = factory(super.-(op))
  override def *(d: Double) = factory(super.*(d))
  override def /(d: Double) = factory(super./(d))

}
