package com.github.agetakoyaki29.scala.geometry.dim2.point.aabb

import com.github.agetakoyaki29.scala.geometry.Delta
import com.github.agetakoyaki29.scala.geometry.Delta._
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2Factory
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2._
import com.github.agetakoyaki29.scala.geometry.dim2.point
import point.Point
import point.Trans
import point.line.{Dir, Line}


object Slab {
  def apply(idx: Int, _sd: Double, border: Border) = new Slab(idx, _sd, border)
  def apply(idx: Int, _sd: Double, _value: Double) = new Slab(idx, _sd, Border(idx, _value))

  def whole(idx: Int) = new Slab(idx, 0, Border.whole(idx)) {
    override def contain(pt: Point) = true
  }
}


class Slab protected (val idx: Int, _sd: Double, val border: Border) {

  val sp = Point(Dim2.E(idx)) * _sd

  def lines: Seq[Line] = ???

  def through(pt: Point): Boolean = ???

  def contain(pt: Point): Boolean = ???

  def distance(pt: Point): Double = ???
  def distanceSqr(pt: Point): Double = ???

  def nearest(pt: Point): Point = ???

  // ----

  def intersect(line: Line) = line intersect this
  def isIntersect(line: Line) = line isIntersect this

}


object Border {
  def apply(idx: Int, _value: Double) = new Border(idx, _value)

  val length = 2

  def whole(idx: Int) = new Border(idx, Double.PositiveInfinity) {
    override def contain(pt: Point) = true
  }
}


class Border(val idx: Int, _value: Double) {
  if(idx < 0 || Border.length <= idx) throw new IllegalArgumentException("idx out of length")

  val value = _value.abs

  def lines: Seq[Line] = Seq(Line(Point(Dim2.E(idx))*value, Dir(Dim2.E(Dim2.other(idx)))))

  // ----

  def through(pt: Point): Boolean = Delta.eq(pt(idx).abs, value)

  def contain(pt: Point): Boolean = Delta.lt(pt(idx).abs, value)

  def distance(pt: Point): Double = (pt(idx).abs - value).abs
  def distanceSqr(pt: Point): Double = distance(pt) ^ 2

  def nearest(pt: Point): Point = pt.updated(idx, Math.copySign(value, pt(idx)))

}
