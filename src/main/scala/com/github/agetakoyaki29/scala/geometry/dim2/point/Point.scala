package com.github.agetakoyaki29.scala.geometry.dim2.point

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

  def localize[T](trans: Trans[T]): T = trans localizedBy this
  def unlocalize[T](trans: Trans[T]): T = trans unlocalizedBy this

  // ---- for trans ----

  def +(op: Point) = this.+(Vector(op))
  def -(op: Point) = this.-(Vector(op))

  // ---- UpRet ----

  override def mapD2(f: Double => Double) = factory(super.mapD2(f))
  override def zipmap(op: Dim2)(f: (Double, Double) => Double) = factory(super.zipmap(op)(f))
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


trait Trans[Repr] {
  def +(pt: Point): Repr
  def -(pt: Point): Repr
  def localizedBy(pt: Point): Repr = this-pt
  def unlocalizedBy(pt: Point): Repr = this+pt
}
