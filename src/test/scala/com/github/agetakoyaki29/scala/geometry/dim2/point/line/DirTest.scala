//package com.github.agetakoyaki29.scala.geometry.dim2.line
//
//import org.scalatest.FunSuite
//
//import com.github.agetakoyaki29.geometry.Delta
//import com.github.agetakoyaki29.geometry.dim2.Point
//
//
//class Dir2Test extends FunSuite {
//  val dir1 = Dir(3, 4)
//  val dir2 = Dir(7, -3)
//  val pt1 = Point(-5, 1)
//  
//  test("distance sqr = distanceSqr") {
//    
//  }
//  test("nearestIsOn") {
//    nearestIsOn(dir1, pt1)
//  }
//  test("normalizedNormIs1") {
//    normalizedNormIs1(dir1)
//  }
//  test("normalIsNormal") {
//    normalIsNormal(dir1)
//  }
//  test("normalIsPlus90Degree") {
//    normalIsPlus90Degree(dir1)
//    normalIsPlus90Degree(Dir(pt1))
//  }
//  test("testCosAngle") {
//    testCosAngle(dir1, dir2)
//  }
//  test("testSinAngle") {
//    testSinAngle(dir1, dir2)
//  }
//
//  def distdist(dir: Dir, pt: Point) = {
//    val distance = dir.distance(pt)
//    val distanceSqr = dir.distanceSqr(pt)
//    val t1 = Delta.eq(distance, distanceSqr)
//    assert(t1)
//  }
//  def nearestIsOn(dir: Dir, pt: Point) = {
//    val near = dir.nearest(pt)
//    val t1 = dir.isOnWithDelta(near)
//    // dir nearest is on dir
//    assert(t1)
//  }
//
//  def normalizedNormIs1(dir: Dir) = {
//    val norm = dir.normalize.norm
////    assertEquals(1, norm, Delta.delta)
//  }
//
//  def normalIsNormal(dir: Dir) = {
//    val normal = dir.normal
//    val t1 = dir.isNormal(normal)
//    // "dir normal is normal by dir"
//    assert(t1)
//  }
//
//  def normalIsPlus90Degree(dir: Dir) = {
//    val normal = dir.normal
//    val angle = dir.angleTo(normal)
//    val normalized = mod(angle, 2*Math.PI)
////    assertEquals(Math.PI/2, normalized, Delta.delta)
//  }
//
//  def mod(d1: Double, d2: Double) = (d1 % d2 + d2) % d2
//
//  def testCosAngle(dir1: Dir, dir2: Dir) = {
//    val deg = dir1 angleTo dir2
//    val cos = dir1 cosTo dir2
//    val delta = Delta.deltaMin(cos, Math.cos(deg))
////    assertEquals(cos, Math.cos(deg), delta)
//  }
//
//  def testSinAngle(dir1: Dir, dir2: Dir) = {
//    val deg = dir1 angleTo dir2
//    val sin = dir1 sinTo dir2
////    assertEquals(sin, Math.sin(deg), Delta.delta)
//  }
//
//}
