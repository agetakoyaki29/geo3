package com.github.agetakoyaki29.scala.geometry.dim2.point.rect

import com.github.agetakoyaki29.scala.geometry.dim2.point
import point.Point
import point.Trans
import point.line.{Dir, Line}
import point.aabb.Slab


case class Rect(sp: Point, corner: Corner) extends Trans[Rect] {

  def updated(sp: Point, corner: Corner) = Rect(sp, corner)
  def updatedSP(sp: Point) = updated(sp, corner)
  def updatedCorner(corner: Corner) = updated(sp, corner)

  // ----

  def +(pt: Point): Rect = updatedSP(sp + pt)
  def -(pt: Point): Rect = updatedSP(sp - pt)

  def *(d: Double): Rect = updated(sp * d, corner * d)
  def /(d: Double): Rect = updated(sp / d, corner / d)

  // ----

  def leftup = sp unlocalize (corner leftup)
  def leftdown = sp unlocalize (corner leftdown)
  def rightup = sp unlocalize (corner rightup)
  def rightdown = sp unlocalize (corner rightdown)
  def center = sp unlocalize (corner center)

  def slab(idx: Int): Slab = corner.slab(idx)
  def slabs: Seq[Slab] = corner.slabs
  def xSlab: Slab = corner.xSlab
  def ySlab: Slab = corner.ySlab

  // ----

  def through(pt: Point): Boolean = sp unlocalize (corner through (sp localize pt))
  def contain(pt: Point): Boolean = sp unlocalize (corner contain (sp localize pt))
  def contain(idx: Int, pt: Point): Boolean = sp unlocalize (corner.contain(idx, sp localize pt))

  def distance(pt: Point): Double = sp unlocalize (corner distance (sp localize pt))
  def distanceSqr(pt: Point): Double = sp unlocalize (corner distanceSqr (sp localize pt))

  def nearest(pt: Point): Point = sp unlocalize (corner nearest (sp localize pt))

  // ----

  def intersect(line: Line): Seq[Point] = line intersect this
  def isIntersect(line: Line): Boolean = line isIntersect this

}
