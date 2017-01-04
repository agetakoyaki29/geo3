package com.github.agetakoyaki29.scala.geometry.dim2.point.line

import com.github.agetakoyaki29.scala.sameret.SameRet
import com.github.agetakoyaki29.scala.sameret.UpRet
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2Factory
import com.github.agetakoyaki29.scala.geometry.dim2.Vector
import com.github.agetakoyaki29.scala.geometry.dim2.point.Point
import com.github.agetakoyaki29.scala.geometry.Delta
import Delta._


object Dir extends Dim2Factory[Dir] {
  def apply(x: Double, y: Double) = new Dir(x, y)

  def angle(angle: Double) = this(Math.cos(angle), Math.sin(angle))

  def chompAngle(angle: Double) = {
    val na = angle % 2*Math.PI
    if(na > Math.PI) na - 2*Math.PI
    else if(na <= -Math.PI) na + 2*Math.PI
    else na
  }
}


@SameRet
class Dir protected (x: Double, y: Double) extends Point(x, y) {

  override def factory: Dim2Factory[_ <: Dir] = Dir

  // ---- validation ----

  override def validate = {
    super.validate
    if(x == 0 && y == 0) throw new IllegalArgumentException("Not Zero")   // this.map{_==0}.reduceLeft{_&&_}
  }

  override def validateElement(d: Double) = NotInfinite orElse AllDouble andThen super.validateElement apply d

  // ----

  def through(pt: Point): Boolean = this dotEq0 pt

  def inRegion1(pt: Point) = this dotGt0 pt
  def inRegion2(pt: Point) = -this dotGt0 pt-this

  /**
   * (this sinTo pt) < 0
   */
  def contain(pt: Point): Boolean = this crossLt0 pt

  /**
   * distance
   * this sinTo pt * pt.norm
   */
  override def distance(pt: Point): Double = (this cross pt / this.norm).abs
  override def distanceSqr(pt: Point): Double = Math.pow(this cross pt, 2) / this.normSqr

  /**
   * nearest point
   * this.normalized * this cosTo pt
   * pt + this.normal.normalized * -distance
   */
  def nearest(pt: Point): Point = Point(this * ((this dot pt) / this.normSqr / pt.norm))

  //

  def same(op: Dir): Boolean = this parallel op

  // def same(line: Line): Boolean = (this passThrough line.sp) && (this parallel line.dir)

  // def aabb: AABB = AABB.WHOLE

  // def intersect(line: Line): Seq[Point] = {

  // def isIntersect(line: Line): Boolean = !(this parallel line.dir)

  // ----

  @UpRet
  def normalized: Dir = this / this.norm

  @UpRet
  def normalDir: Dir = factory(-y, x)

  def normal(op: Dir): Boolean = this dotEq0 op

  def parallel(op: Dir): Boolean = this crossEq0 op

  // ----

  /**
   * -pi ~ pi
   */
  def angle: Double = Math.atan2(y, x)

  def angleTo(op: Dir): Double = op.angle - this.angle

  def cosTo(op: Dir): Double = this dot op / this.norm / op.norm

  def sinTo(op: Dir): Double = this cross op / this.norm / op.norm

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
