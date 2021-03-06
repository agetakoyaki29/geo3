package com.github.agetakoyaki29.scala.geometry.dim2

import com.github.agetakoyaki29.scala.sameret.SameRet
import com.github.agetakoyaki29.scala.sameret.UpRet
import com.github.agetakoyaki29.scala.geometry.Delta
import Delta._


object Vector extends Dim2Factory[Vector] {
  def apply(x: Double, y: Double) = new Vector(x, y)
}


@SameRet
class Vector protected (x: Double, y: Double) extends Dim2(x, y) {

  override def factory: Dim2Factory[_ <: Vector] = Vector

  @UpRet
  def abs: Vector = mapD2{_.abs}

  @UpRet
  def unary_+(): Vector = factory(this)
  @UpRet
  def unary_-(): Vector = mapD2{-_}

  @UpRet
  def +(op: Vector): Vector = zipmapD2(op){_+_}
  @UpRet
  def -(op: Vector): Vector = zipmapD2(op){_-_}

  @UpRet
  def *(d: Double): Vector = mapD2{_*d}
  @UpRet
  def /(d: Double): Vector = mapD2{_/d}

  final def dot(op: Vector) = zipmapD2(op){_*_}.reduceLeft{_+_}
  final def dotEq0(op: Vector) = Delta.eq(x*op.x, -y*op.y)
  final def dotGt0(op: Vector) = Delta.gt(x*op.x, -y*op.y)
  final def dotLt0(op: Vector) = Delta.lt(x*op.x, -y*op.y)

  final def cross(op: Vector) = x*op.y - y*op.x
  final def crossEq0(op: Vector) = Delta.eq(x*op.y, y*op.x)
  final def crossGt0(op: Vector) = Delta.gt(x*op.y, y*op.x)
  final def crossLt0(op: Vector) = Delta.lt(x*op.y, y*op.x)

  def norm: Double = normSqr.sqrt
  def normSqr: Double = this dot this

  // ---- UpRet ----

  override def mapD2(f: Double => Double) = factory(super.mapD2(f))
  override def zipmapD2(op: Dim2)(f: (Double, Double) => Double) = factory(super.zipmapD2(op)(f))
  override def updated(x: Double, y: Double) = factory(super.updated(x, y))
  override def updatedX(x: Double) = factory(super.updatedX(x))
  override def updatedY(y: Double) = factory(super.updatedY(y))

}
