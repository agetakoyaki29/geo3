package com.github.agetakoyaki29.scala.geometry.dim2

import com.github.agetakoyaki29.scala.geometry.Delta._


object Vector extends Dim2Factory[Dim2] {
  def apply(x: Double, y: Double) = new Vector(x, y)
}

class Vector protected (x: Double, y: Double) extends Dim2(x, y) {
  
}