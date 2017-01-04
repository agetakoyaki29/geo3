package com.github.agetakoyaki29.scala.geometry.dim2.point


trait Trans[Repr] {
  def +(pt: Point): Repr
  def -(pt: Point): Repr
  def localizedBy(pt: Point): Repr = this-pt
  def unlocalizedBy(pt: Point): Repr = this+pt
}
