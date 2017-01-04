package com.github.agetakoyaki29.scala.geometry.dim2.point.circle

import com.github.agetakoyaki29.scala.geometry.dim2.point
import point.Point
import point.Trans
import point.line.{Dir, Line}


case class Circle(sp: Point, range: Range) extends Trans[Circle] {

  def updated(sp: Point, range: Range) = Circle(sp, range)
  def updatedSP(sp: Point) = updated(sp, range)
  def updatedRange(range: Range) = updated(sp, range)

  // ----

  def +(pt: Point): Circle = updatedSP(sp + pt)
  def -(pt: Point): Circle = updatedSP(sp - pt)

  def *(d: Double): Circle = updated(sp * d, range * d)
  def /(d: Double): Circle = updated(sp / d, range / d)

  // ----

  def through(pt: Point): Boolean = ???
  def contain(pt: Point): Boolean = ???

  def distance(pt: Point): Double = ???
  def distanceSqr(pt: Point): Double = ???

  def nearest(pt: Point): Point = ???

  // ----

  def intersect(line: Line): Seq[Point] = ???
  def isIntersect(line: Line): Boolean = ???

}
