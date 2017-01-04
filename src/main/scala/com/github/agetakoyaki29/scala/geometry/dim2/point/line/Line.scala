package com.github.agetakoyaki29.scala.geometry.dim2.point.line

import com.github.agetakoyaki29.scala.geometry.dim2.point
import point.Point
import point.Trans


case class Line(sp: Point, dir: Dir) extends Trans[Line] {

  def updated(sp: Point, dir: Dir) = Line(sp, dir)
  def updatedSP(sp: Point) = updated(sp, dir)
  def updatedDir(dir: Dir) = updated(sp, dir)

  // ----

  def +(pt: Point): Line = updatedSP(sp + pt)
  def -(pt: Point): Line = updatedSP(sp - pt)

  def *(d: Double): Line = updated(sp * d, dir * d)
  def /(d: Double): Line = updated(sp / d, dir / d)

  // ----

  def through(pt: Point): Boolean = ???
  def inRegion1(pt: Point): Boolean = ???
  def inRegion2(pt: Point): Boolean = ???
  def contain(pt: Point): Boolean = ???

  def distance(pt: Point): Double = ???
  def distanceSqr(pt: Point): Double = ???

  def nearest(pt: Point): Point = ???

  // ----

  def intersect(line: Line): Seq[Point] = ???
  def isIntersect(line: Line): Boolean = ???

  // ----

  def reverse: Line = updated(sp+dir, dir.reverse)
  def normalized: Line = ???
  def normalDir: Line = ???
  def normal(line: Line): Boolean = ???
  def parallel(line: Line): Boolean = ???

  // ----

  def angle: Double = ???
  def angleTo(line: Line): Double = ???
  def cosTo(line: Line): Double = ???
  def sinTo(line: Line): Double = ???

}
