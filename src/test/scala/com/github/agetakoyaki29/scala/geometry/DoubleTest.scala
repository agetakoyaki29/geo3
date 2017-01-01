package com.github.agetakoyaki29.scala.geometry

import org.scalatest.WordSpec


class DoubleTest extends WordSpec {
  "Double.NaN" should {
    "produce IllegalArgumentException thr Delta.NotNaN" in {
      intercept[IllegalArgumentException] {
        Delta.NotNaN(Double.NaN)
      }
    }
  }
  
  "Double.MinPositiveValue" should {
    "produce IllegalArgumentException thr Delta.NotMinPositiveValue" in {
      intercept[IllegalArgumentException] {
        Delta.NotMinPositiveValue(Double.MinPositiveValue)
      }
    }
  }
  
  "Double.PositiveInfinity" should {
    "produce IllegalArgumentException thr Delta.NotInfinite" in {
      intercept[IllegalArgumentException] {
        Delta.NotInfinite(Double.PositiveInfinity)
      }
    }
  }
  
  "Double.NegativeInfinity" should {
    "produce IllegalArgumentException thr Delta.NotInfinite" in {
      intercept[IllegalArgumentException] {
        Delta.NotInfinite(Double.NegativeInfinity)
      }
    }
  }
  
  "0.0d" should {
    "produce IllegalArgumentException thr Delta.NotZero" in {
      intercept[IllegalArgumentException] {
        Delta.NotZero(0.0d)
      }
    }
  }
  
}