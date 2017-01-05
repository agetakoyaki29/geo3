package com.github.agetakoyaki29.scala.geometry.dim2.point.rect

import com.github.agetakoyaki29.scala.geometry.dim2.point
import point.Point
import point.Trans
import point.line.{Dir, Line}


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

  def leftup = ???
  def leftdown = ???
  def rightup = ???
  def rightdown = ???
  def center = ???

  def slab(idx: Int): Seq[Line] = ???
  def xSlab: Seq[Line] = ???
  def ySlab: Seq[Line] = ???

  // ----

  def through(pt: Point): Boolean = ???
  def contain(pt: Point): Boolean = ???
  def contain(idx: Int, pt: Point): Boolean = ???

  def distance(pt: Point): Double = ???
  def distanceSqr(pt: Point): Double = ???

  def nearest(pt: Point): Point = ???

  // ----

  def intersect(line: Line): Seq[Point] = line intersect this
  def isIntersect(line: Line): Boolean = line isIntersect this

}
