package com.github.agetakoyaki29.scala.geometry.dim2

import org.scalatest.WordSpec
//import org.scalatest.Assertions._


class Dim2Test extends WordSpec {

  "A Dim2" when {
    "have NaN Element" should {
      "produce IllegalArgumentException when construct" in {
        intercept[IllegalArgumentException] {
          Dim2(2.2, Double.NaN)
        }
      }
    }

    "Dim2.ZERO" should {
      "isZero" in {
        assert(Dim2.ZERO.isZero)
      }
    }

    "Dim2.INFINITY" should {
      "isInfinite" in {
        assert(Dim2.INFINITY.isInfinite)
      }
    }
  }

}
