package com.github.agetakoyaki29.scala.geometry.dim2.point

import com.github.agetakoyaki29.scala.sameret.SameRet
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2
import com.github.agetakoyaki29.scala.geometry.dim2.Dim2Factory
import com.github.agetakoyaki29.scala.geometry.dim2.Vector


object Point extends Dim2Factory[Point] {
  def apply(x: Double, y: Double) = new Point(x, y)
}


@SameRet
class Point protected (x: Double, y: Double) extends Vector(x, y) {

  override def factory: Dim2Factory[_ <: Point] = Point

  def distance(op: Point) = Math.sqrt(distanceSqr(op))
  def distanceSqr(op: Point) = (this-op).normSqr  
  
  // ---- UpRet ----
  
  override def mapD2(f: Double => Double) = factory(super.mapD2(f))
  override def zipmap(op: Dim2)(f: (Double, Double) => Double) = factory(super.zipmap(op)(f))

  override def abs = factory(super.abs)
  override def unary_+() = factory(super.unary_+)
  override def unary_-() = factory(super.unary_-)
  override def +(op: Vector) = factory(super.+(op))
  override def -(op: Vector) = factory(super.-(op))
  override def *(d: Double) = factory(super.*(d))
  override def /(d: Double) = factory(super./(d))
  
}