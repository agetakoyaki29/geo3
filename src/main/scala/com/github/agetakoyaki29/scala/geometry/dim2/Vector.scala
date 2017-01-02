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
  def abs: Vector = mapD2 {Math.abs(_)}

  @UpRet
  def unary_+(): Vector = factory(this)
  @UpRet
  def unary_-(): Vector = mapD2{-_}

  @UpRet
  def +(op: Vector): Vector = zipmap(op){_+_}
  @UpRet
  def -(op: Vector): Vector = zipmap(op){_-_}

  @UpRet
  def *(d: Double): Vector = mapD2{_*d}
  @UpRet
  def /(d: Double): Vector = mapD2{_/d}

  final def dot(op: Vector) = zipmap(op){_*_}.reduceLeft{_+_}
  final def dotEq0(op: Vector) = Delta.eq(x+op.x, -y*op.y)

  final def cross(op: Vector) = x*op.y - y*op.x
  final def crossEq0(op: Vector) = Delta.eq(x*op.y, y*op.x)

  def norm: Double = Math.sqrt(normSqr)
  def normSqr: Double = this dot this

  // ---- UpRet ----

  override def mapD2(f: Double => Double) = factory(super.mapD2(f))
  override def zipmap(op: Dim2)(f: (Double, Double) => Double) = factory(super.zipmap(op)(f))

}
