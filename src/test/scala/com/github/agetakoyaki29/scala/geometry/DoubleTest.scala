package com.github.agetakoyaki29.scala.geometry

import org.scalatest.WordSpec


class DoubleTest extends WordSpec {
  
  "Double.NaN" should {
    "produce IllegalArgumentException thr Delta.NotNaN" in {
      intercept[IllegalArgumentException] {
        Delta.NotNaN(Double.NaN)
      }
    }
    "NaN is not other NaN" in {
      assert(Double.NaN !== (Double.NaN + 23))
      assert(Double.NaN !== Double.NaN)
    }
    "Four arithmetic operations result isNaN" in {
      assert((Double.NaN + 2).isNaN)
      assert((Double.NaN - 3).isNaN)
      assert((Double.NaN * 4).isNaN)
      assert((Double.NaN / 5).isNaN)
      assert((6 / Double.NaN).isNaN)
    }
  }
  
  "Double.MinPositiveValue" should {
//    "produce IllegalArgumentException thr Delta.NotMinPositiveValue" in {
//      intercept[IllegalArgumentException] {
//        Delta.NotMinPositiveValue(Double.MinPositiveValue)
//      }
//    }
    "Four arithmetic operations1" in {
      assert((Double.MinPositiveValue + 2.1) === 2.1)
      assert((Double.MinPositiveValue + 3.2) === 3.2)
    }
    "Four arithmetic operations2" in {
      assert((Double.MinPositiveValue * 1.1) === Double.MinPositiveValue)
      assert((Double.MinPositiveValue * 2) === 1.0e-323)
    }
    "Four arithmetic operations3" in {
      assert((Double.MinPositiveValue / 5) === 0)
    }
    "Four arithmetic operations4" in {
      assert((54.4 / Double.MinPositiveValue) === Double.PositiveInfinity)
    }
  }
  
  "Infinity" should {
    "produce IllegalArgumentException thr Delta.NotInfinite" in {
      intercept[IllegalArgumentException] {
        Delta.NotInfinite(Double.PositiveInfinity)
      }
      intercept[IllegalArgumentException] {
        Delta.NotInfinite(Double.NegativeInfinity)
      }
    }
    "Four arithmetic operations1" in {
      assert((Double.PositiveInfinity + 51.31) === Double.PositiveInfinity)
      assert((Double.PositiveInfinity - 351.31) === Double.PositiveInfinity)
    }
    "Four arithmetic operations2" in {
      assert((Double.PositiveInfinity * 3.55) === Double.PositiveInfinity)
      assert((Double.PositiveInfinity / 413.4) === Double.PositiveInfinity)
    }
    "Four arithmetic operations3" in {
      assert((Double.PositiveInfinity * -3.3) === Double.NegativeInfinity)
      assert((Double.NegativeInfinity * -24.3) === Double.PositiveInfinity)
    }
    "Four arithmetic operations4" in {
      assert((113.3 / Double.PositiveInfinity) === 0d)
      assert((6.23  / Double.NegativeInfinity) === 0d)
    }
  }
  
  "0.0d" should {
    "produce IllegalArgumentException thr Delta.NotZero" in {
      intercept[IllegalArgumentException] {
        Delta.NotZero(0d)
      }
    }
    "Four arithmetic operations1" in {
      assert((1.4 / 0d) === Double.PositiveInfinity)
      assert((-2.4 / 0d) === Double.NegativeInfinity)
    }
  }
  
}