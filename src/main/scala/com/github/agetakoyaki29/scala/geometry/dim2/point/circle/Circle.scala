package com.github.agetakoyaki29.scala.geometry.dim2.point.circle

import com.github.agetakoyaki29.scala.geometry.dim2.point
import point.Point
import point.Trans
import point.line.{Dir, Line}


object Circle {
  def apply(sp: Point, ep: Point) = new Circle(sp, Range(sp localize ep))
}

case class Circle(sp: Point, range: Range) extends Trans[Circle] {

  val ep: Point = sp unlocalize range

  def updated(sp: Point, range: Range) = Circle(sp, range)
  def updatedSP(sp: Point) = updated(sp, range)
  def updatedEP(ep: Point) = updated(sp, Range(sp localize ep))
  def updatedRange(range: Range) = updated(sp, range)

  // ----

  def +(pt: Point): Circle = updatedSP(sp + pt)
  def -(pt: Point): Circle = updatedSP(sp - pt)

  def *(d: Double): Circle = updated(sp * d, range * d)
  def /(d: Double): Circle = updated(sp / d, range / d)

  def radicalLine(circle: Circle): Line = sp unlocalize (range radicalLine (sp localize circle))

  // ----

  def through(pt: Point): Boolean = sp unlocalize (range through (sp localize pt))
  def contain(pt: Point): Boolean = sp unlocalize (range contain (sp localize pt))

  def distance(pt: Point): Double = sp unlocalize (range distance (sp localize pt))
  def distanceSqr(pt: Point): Double = sp unlocalize (range distanceSqr (sp localize pt))

  def nearest(pt: Point): Point = sp unlocalize (range nearest (sp localize pt))

  // ----

  def same(circle: Circle): Boolean = sp unlocalize (range same (sp localize circle))

  def intersect(line: Line): Seq[Point] = (range intersect (sp localize line)) map {sp unlocalize _}
  def isIntersect(line: Line): Boolean = sp unlocalize (range isIntersect (sp localize line))

  def intersect(circle: Circle): Seq[Point] = (range intersect (sp localize circle)) map {sp unlocalize _}
  def isIntersect(circle: Circle): Boolean = sp unlocalize (range isIntersect (sp localize circle))

}
