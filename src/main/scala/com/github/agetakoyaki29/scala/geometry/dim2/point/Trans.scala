package com.github.agetakoyaki29.scala.geometry.dim2.point


object Trans {
  implicit class TransSeq[A <: Trans[A]](it: Seq[A]) extends Trans[TransSeq[A]] {
    def +(pt: Point): TransSeq[A] = it.map{_+pt}
    def -(pt: Point): TransSeq[A] = it.map{_-pt}
  }
}


trait Trans[Repr] {
  def +(pt: Point): Repr
  def -(pt: Point): Repr
  def localizedBy(pt: Point): Repr = this-pt
  def unlocalizedBy(pt: Point): Repr = this+pt
}
