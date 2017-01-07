package com.github.agetakoyaki29.scala.geometry.dim2.point.circle

import com.github.agetakoyaki29.scala.sameret.SameRet
import com.github.agetakoyaki29.scala.sameret.UpRet
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2Factory
import com.github.agetakoyaki29.scala.geometry.dim2.Vector
import com.github.agetakoyaki29.scala.geometry.dim2.point.Point
import com.github.agetakoyaki29.scala.geometry.dim2.point.line.{Dir, Line}
import com.github.agetakoyaki29.scala.geometry.Delta
import Delta._


object Range extends Dim2Factory[Range] {
  def apply(x: Double, y: Double) = new Range(x, y)
}


@SameRet
class Range protected (x: Double, y: Double) extends Point(x, y) {

  override def factory: Dim2Factory[_ <: Range] = Range

  // ----

  /**
   * sp.normalized * (this.normSqr - op.normSqr + sp.normSqr) / (2*sp.norm)
   */
  def radicalLine(circle: Circle): Line = {
    val op = circle.range
    val distanceSqr = circle.sp.normSqr
    val a = ((this.normSqr/distanceSqr) - (op.normSqr/distanceSqr) + 1) / 2
    Line(circle.sp * a, Dir(circle.sp).normalDir)
  }

  // ----

  def through(pt: Point): Boolean = Delta.eq(pt.normSqr, this.normSqr)

  def contain(pt: Point) = Delta.lt(pt.normSqr, this.normSqr)

  override def distance(pt: Point): Double = (pt.norm-this.norm).abs

  override def distanceSqr(pt: Point): Double = distance(pt)^2

  def nearest(pt: Point): Point = pt * (this.norm/pt.norm)

  // ----

  // def aabb = {
  //   val norm = this.norm
  //   AABB(Point.ORIGIN, Corner(norm, norm))
  // }

  def intersect(line: Line): Seq[Point] = {
    val nearest = line.nearest(Point.ORIGIN)
    if(!(this contain nearest)) Seq()
    else if(this through nearest) Seq(nearest)
    else {
      val diff = (this.normSqr - nearest.normSqr).sqrt
      val dd = line.dir.normalized * diff
      Seq(nearest-dd, nearest+dd)
    }
  }
  def isIntersect(line: Line): Boolean = this contain line.nearest(Point.ORIGIN)

  def intersect(circle: Circle): Seq[Point] = this intersect radicalLine(circle)
  def isIntersect(circle: Circle): Boolean = Delta.lt(Point.ORIGIN distanceSqr circle.sp, this.normSqr + circle.range.normSqr)

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
