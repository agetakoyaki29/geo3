package com.github.agetakoyaki29.scala.geometry.dim2.point

import org.scalatest.WordSpec

import com.github.agetakoyaki29.scala.geometry.dim2.point
import point.line.Line
import point.line.Dir


class PointTest extends WordSpec {
  val pt = Point(2, 3)

  "Point.localize" when {
    "any(non trans)" should {
      val any = "asada"
      "return same any" in {
        assert(any === pt.localize(any))
      }
    }
    "trans" should {
      val trans = Line(Point(3, 4), Dir(5, 6))
      "return trans" in {
        assert(trans.getClass isInstance pt.localize(trans))
        assert(trans !== pt.localize(trans))
      }
    }
    "seq" should {
      val seq1 = Seq("a", "bo")
      val seq2 = Seq(Point(7, 8), Point(9, 0))
      "return seq" in {
        assert(seq1 === pt.localize(seq1))
        println(seq1.getClass)
        println(seq2.getClass)
        assert(seq1.getClass !== pt.localize(seq2).getClass)
        assert(seq2 !== pt.localize(seq2))
      }
    }
  }
  "Point.fff" when {
    val f0: Boolean => String = {_.toString}
    val f1: Point => Point = {_*2}
    val f2: Point => Seq[Point] = {(pt: Point) => Seq(pt*2)}
    "com(Any)" in {
      assert(pt.fff(f0).apply(true) === "true")
    }
    "com" in {
      assert(pt.fff(f1).apply(pt) === pt)
      assert(pt.fff(f1).apply(pt+Point(1, 0)) === pt+Point(2, 0))
    }
    "com(Seq)" in {
      assert(pt.fff(f2).apply(pt) === Seq(pt))
      assert(pt.fff(f2).apply(pt+Point(1, 0)) === Seq(pt+Point(2, 0)))
    }
  }
}
