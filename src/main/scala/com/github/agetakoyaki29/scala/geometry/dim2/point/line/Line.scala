package com.github.agetakoyaki29.scala.geometry.dim2.point.line

import com.github.agetakoyaki29.scala.geometry.dim2.point
import point.Point
import point.Trans
import point.aabb.Slab
import point.rect.Rect
import point.circle.Circle


object Line {
  def apply(sp: Point, dir: Dir) = new Line(sp, dir)
  def apply(sp: Point, ep: Point) = new Line(sp, Dir(sp localize ep))

  def align(i: Int, d: Double) = {
    val j = (i+1) % 2
    new Line(Point.ORIGIN.updated(j, d), Dir.align(i))
  }
  def xAlign(y: Double) = align(0, y)
  def yAlign(x: Double) = align(1, x)
}

class Line(val sp: Point, val dir: Dir) extends Trans[Line] {

  val ep: Point = sp unlocalize dir

  def updated(sp: Point, dir: Dir) = Line(sp, dir)
  def updatedSP(sp: Point) = updated(sp, dir)
  def updatedEP(ep: Point) = updated(sp, Dir(sp localize ep))
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

  def same(line: Line): Boolean = ???

  def intersect(line: Line): Seq[Point] = ???
  def isIntersect(line: Line): Boolean = ???

  def intersect(slab: Slab): Seq[Point] = ???
  def isIntersect(slab: Slab): Boolean = ???

  def intersect(rect: Rect): Seq[Point] = ???
  def isIntersect(rect: Rect): Boolean = ???

  def intersect(circle: Circle): Seq[Point] = circle intersect this
  def isIntersect(circle: Circle): Boolean = circle isIntersect this

  // ----

  def align(i: Int): Boolean = ???
  def alignX: Boolean = ???
  def alignY: Boolean = ???
  def reflect: Line = updated(sp+dir, dir.reflect)
  def normalized: Line = ???
  def normalDir: Line = ???
  def normal(line: Line): Boolean = ???
  def parallel(line: Line): Boolean = ???

  // ----

  def angle: Double = ???
  def angleTo(line: Line): Double = ???
  def cosTo(line: Line): Double = ???
  def sinTo(line: Line): Double = ???

  // ---- std ----

  override def toString: String = s"Line(sp: ${sp}, dir: ${dir})"

}
