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

  val POINT = this(Dim2.ZERO)
  val INFINITY = this(Double.PositiveInfinity, Double.PositiveInfinity)
}


@SameRet
class Size protected (x: Double, y: Double) extends Point(x, y) {

  override def factory: Dim2Factory[_ <: Size] = Size

  // ---- validation ----

  override def validateElement(d: Double) = NotMinus orElse AllDouble andThen super.validateElement apply d

  // ----

  def leftup = Point.ORIGIN
  def leftdown = Point(0, y)
  def rightup = Point(x, 0)
  def rightdown = Point(this)
  def center = Point(this/2)

  // ----

  def contain(pt: Point) = containX(pt.x) && containY(pt.y)

  def containX(d: Double) = lt(0, d) && lt(d, x)
  def containY(d: Double) = lt(0, d) && lt(d, y)

  override def distanceSqr(pt: Point): Double = Math.pow(distance(pt), 2)
  override def distance(pt: Point): Double = {
    val (cx, cy) = (containX(pt.x), containY(pt.y))
    val (mx, my) = {
      val sign = pt localizedBy center
      (sign.x <= 0, sign.y <= 0)
    }
    val (nx, ny) = (if(mx) 0 else x, if(my) 0 else y)
    val (dx, dy) = (if(mx) 0-pt.x else pt.x-x, if(my) 0-pt.y else pt.y-y)

    ??? // XXX
  }

  def nearest(pt: Point): Point = {
    val (cx, cy) = (containX(pt.x), containY(pt.y))
    val (mx, my) = {
      val sign = pt localizedBy center
      (sign.x <= 0, sign.y <= 0)
    }
    val (nx, ny) = (if(mx) 0 else x, if(my) 0 else y)

    if(cx && cy) {
      ??? // XXX
    }
    else if(cx) return pt.updatedY(ny)
    else if(cy) return pt.updatedX(nx)
    else return pt.updated(nx, ny)
  }

  def through(pt: Point): Boolean = {
    if(Delta.eq(pt.x, 0) || Delta.eq(pt.x, x))  containY(pt.y)
    else if(Delta.eq(pt.y, 0) || Delta.eq(pt.y, y)) containX(pt.x)
    else false
  }

  // ---- UpRet ----

  override def mapD2(f: Double => Double) = factory(super.mapD2(f))
  override def zipmapD2(op: Dim2)(f: (Double, Double) => Double) = factory(super.zipmapD2(op)(f))
  override def updated(x: Double, y: Double) = factory(super.updated(x, y))
  override def updatedX(x: Double) = factory(super.updatedX(x))
  override def updatedY(y: Double) = factory(super.updatedY(y))

  override def abs = factory(super.abs)
  override def unary_+() = factory(super.unary_+)
  override def unary_-() = factory(super.unary_-)   // Unsuport
  override def +(op: Vector) = factory(super.+(op))
  override def -(op: Vector) = factory(super.-(op))
  override def *(d: Double) = factory(super.*(d))
  override def /(d: Double) = factory(super./(d))

}
