package com.github.agetakoyaki29.scala.geometry.dim2.point.aabb

import com.github.agetakoyaki29.scala.sameret.SameRet
import com.github.agetakoyaki29.scala.sameret.UpRet
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2Factory
import com.github.agetakoyaki29.scala.geometry.dim2.Vector
import com.github.agetakoyaki29.scala.geometry.dim2.point.Point
import com.github.agetakoyaki29.scala.geometry.Delta
import Delta._


object Size extends Dim2Factory[Size] {
  def apply(x: Double, y: Double) = new Size(x, y)

  val POINT = ???
  val INFINITY = ???
}


@SameRet
class Size protected (x: Double, y: Double) extends Point(x, y) {

  override def factory: Dim2Factory[_ <: Size] = Size

  // ---- validation ----

  override def validateElement(d: Double) = NotMinus orElse AllDouble andThen super.validateElement apply d

  // ----

  def contain(pt: Point) = {
    gt(0, pt.x) && gt(pt.x, x) &&
    gt(0, pt.y) && gt(pt.y, y)
  }

  override def distance(pt: Point): Double = ???
  override def distanceSqr(pt: Point): Double = ???

  def nearest(pt: Point): Point = ???

  def through(pt: Point): Boolean = {
    if(Delta.eq(pt.x, 0) || Delta.eq(pt.x, x)) gt(0, pt.y) && gt(pt.y, y)
    else if(Delta.eq(pt.y, 0) || Delta.eq(pt.y, y)) gt(0, pt.x) && gt(pt.x, x)
    else false
  }

  // ---- UpRet ----

  override def mapD2(f: Double => Double) = factory(super.mapD2(f))
  override def zipmap(op: Dim2)(f: (Double, Double) => Double) = factory(super.zipmap(op)(f))

  override def abs = factory(super.abs)
  override def unary_+() = factory(super.unary_+)
  override def unary_-() = factory(super.unary_-)   // Unsuport
  override def +(op: Vector) = factory(super.+(op))
  override def -(op: Vector) = factory(super.-(op))
  override def *(d: Double) = factory(super.*(d))
  override def /(d: Double) = factory(super./(d))

}
