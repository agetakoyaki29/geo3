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

  implicit def dirToLine(dir: Dir): Line = Line(Point.ORIGIN, dir)
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

  def reflect: Line = updated(sp+dir, dir.reflect)

  // ----

  def through(pt: Point): Boolean = sp unlocalize (dir through (sp localize pt))
  def inRegion1(pt: Point): Boolean = sp unlocalize (dir inRegion1 (sp localize pt))
  def inRegion2(pt: Point): Boolean = sp unlocalize (dir inRegion2 (sp localize pt))
  def contain(pt: Point): Boolean = sp unlocalize (dir contain (sp localize pt))

  def distance(pt: Point): Double = sp unlocalize (dir distance (sp localize pt))
  def distanceSqr(pt: Point): Double = sp unlocalize (dir distanceSqr (sp localize pt))

  def nearest(pt: Point): Point = sp unlocalize (dir nearest (sp localize pt))

  // ----

  def same(line: Line): Boolean = sp unlocalize (dir same (sp localize line))

  def intersect(line: Line): Seq[Point] = (dir intersect (sp localize line)) map {sp unlocalize _}
  def isIntersect(line: Line): Boolean = sp unlocalize (dir isIntersect (sp localize line))

  def intersect(slab: Slab): Seq[Point] = (dir intersect (sp localize slab)) map {sp unlocalize _}
  def isIntersect(slab: Slab): Boolean = sp unlocalize (dir isIntersect (sp localize slab))

  def intersect(rect: Rect): Seq[Point] = (dir intersect (sp localize rect)) map {sp unlocalize _}
  def isIntersect(rect: Rect): Boolean = sp unlocalize (dir isIntersect (sp localize rect))

  def intersect(circle: Circle): Seq[Point] = circle intersect this
  def isIntersect(circle: Circle): Boolean = circle isIntersect this

  // ----

  def align(idx: Int): Boolean = sp unlocalize (dir align (sp localize idx))
  def alignX: Boolean = sp unlocalize (dir alignX)
  def alignY: Boolean = sp unlocalize (dir alignY)
  def normalized: Line = sp unlocalize (dir normalized)
  def normalDir: Line = sp unlocalize (dir normalDir)
  def normal(line: Line): Boolean = dir normal line.dir
  def parallel(line: Line): Boolean = dir parallel line.dir

  // ----

  def angle: Double = sp unlocalize (dir angle)
  def angleTo(line: Line): Double = dir angleTo line.dir
  def cosTo(line: Line): Double = dir cosTo line.dir
  def sinTo(line: Line): Double = dir sinTo line.dir

  // ---- std ----

  override def toString: String = s"Line(sp: ${sp}, dir: ${dir})"

}
