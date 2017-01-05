package com.github.agetakoyaki29.scala.geometry.dim2.point.aabb

import com.github.agetakoyaki29.scala.geometry.Delta
import com.github.agetakoyaki29.scala.geometry.Delta._
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2._
import com.github.agetakoyaki29.scala.geometry.dim2.point
import point.Point
import point.Trans
// import point.line.{Border, Line}


object Border {
  val length = 2
}

case class Border(val idx: Int, _value: Double) {
  if(idx < 0 || Border.length <= idx) throw new IllegalArgumentException("idx out of length")

  val value = _value.abs

  // ----

  def through(pt: Point): Boolean = Delta.eq(pt(idx).abs, value)

  def contain(pt: Point): Boolean = Delta.lt(pt(idx).abs, value)

  def distance(pt: Point): Double = (pt(idx).abs - value).abs
  def distanceSqr(pt: Point): Double = distance(pt) ^ 2

  def nearest(pt: Point): Point = pt.updated(idx, Math.copySign(value, pt(idx)))

}
