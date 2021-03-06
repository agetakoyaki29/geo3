package com.github.agetakoyaki29.scala.geometry.dim2.point.rect

import com.github.agetakoyaki29.scala.sameret.SameRet
import com.github.agetakoyaki29.scala.sameret.UpRet
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2Factory
import com.github.agetakoyaki29.scala.geometry.dim2.Vector
import com.github.agetakoyaki29.scala.geometry.dim2.point.Point
import com.github.agetakoyaki29.scala.geometry.dim2.point.line.Line
import com.github.agetakoyaki29.scala.geometry.dim2.point.aabb.Slab
import com.github.agetakoyaki29.scala.geometry.Delta
import Delta._


object Corner extends Dim2Factory[Corner] {
  def apply(x: Double, y: Double) = new Corner(x, y)

  val POINT = new Corner(Dim2.ZERO) {
    override def through(pt: Point) = contain(pt)
    override def contain(pt: Point) = pt same this
    override def containX(d: Double) = Delta.eq(d, 0)
    override def containY(d: Double) = Delta.eq(d, 0)
    override def distance(pt: Point) = Point(this) distance pt
    override def distanceSqr(pt: Point) = Point(this) distanceSqr pt
    override def nearest(pt: Point) = Point(this)
  }
  val INFINITY = new Corner(Double.PositiveInfinity, Double.PositiveInfinity) {
    override def contain(pt: Point) = true
    override def containX(d: Double) = true
    override def containY(d: Double) = true
  }
}


@SameRet
class Corner protected (x: Double, y: Double) extends Point(x.abs, y.abs) {
  def this(dim2: Dim2) = this(dim2.x, dim2.y)

  override def factory: Dim2Factory[_ <: Corner] = Corner

  // ----

  def leftup = Point(-x, -y)
  def leftdown = Point(-x, y)
  def rightup = Point(x, -y)
  def rightdown = Point(this)
  def center = Point.ORIGIN

  def slab(idx: Int): Slab = Slab(idx, 0, this(Dim2.other(idx)))
  def slabs: Seq[Slab] = indices map {slab(_)}
  def xSlab: Slab = slab(0)
  def ySlab: Slab = slab(1)

  // ----

  def through(pt: Point): Boolean = {
    if(Delta.eq(pt.x.abs, x)) containY(pt.y)
    else if(Delta.eq(pt.y.abs, y)) containX(pt.x)
    else false
  }

  def contain(pt: Point): Boolean = Seq(lt(pt.abs.x, x), lt(pt.abs.y, y)) reduceLeft {_&&_}
  def contain(idx: Int, pt: Point): Boolean = lt(pt(idx).abs, this(idx))

  def containX(d: Double) = lt(d.abs, x)
  def containY(d: Double) = lt(d.abs, y)

  override def distance(pt: Point): Double = distanceSqr(pt).sqrt
  override def distanceSqr(pt: Point): Double = {
    val (cx, cy) = (containX(pt.x), containY(pt.y))
    val dp = (pt.abs - this).abs

    if(cx && cy) return Math.min(dp.x, dp.y) ^ 2
    else if(cx) return dp.y ^ 2
    else if(cy) return dp.x ^ 2
    else return dp.normSqr
  }

  def nearest(pt: Point): Point = {
    val (cx, cy) = (containX(pt.x), containY(pt.y))
    val (sx, sy) = {
      val (mx, my) = (pt.x <= 0, pt.y <= 0)
      (if(mx) -1 else 1, if(my) -1 else 1)
    }

    if(cx && cy) {
      val dp = (pt.abs - this).abs
      if(dp.x <= dp.y) return pt.updatedX(sx*x)
      else return pt.updatedY(sy*y)
    }
    else if(cx) return pt.updatedY(sy*y)
    else if(cy) return pt.updatedX(sx*x)
    else return pt.updated(sx*x, sy*y)
  }

  // ----

  // def intersect = ???

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
