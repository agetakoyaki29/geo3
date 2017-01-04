package com.github.agetakoyaki29.scala.geometry.dim2.point.line

import org.scalatest.FunSuite

import com.github.agetakoyaki29.scala.geometry.Delta
import com.github.agetakoyaki29.scala.geometry.dim2.point.Point


class Dir2Test extends FunSuite {
 val dir1 = Dir(3, 4)
 val dir2 = Dir(7, -3)
 val pt1 = Point(-5, 1)

 test("distance sqr = distanceSqr") {

 }
 test("nearestIsOn") {
   nearestIsOn(dir1, pt1)
 }
 test("normalizedNormIs1") {
   normalizedNormIs1(dir1)
 }
 test("normalIsNormal") {
   normalIsNormal(dir1)
 }
 test("normalIsPlus90Degree") {
   normalIsPlus90Degree(dir1)
   normalIsPlus90Degree(Dir(pt1))
 }
 test("testCosAngle") {
   testCosAngle(dir1, dir2)
 }
 test("testSinAngle") {
   testSinAngle(dir1, dir2)
 }

 def distdist(dir: Dir, pt: Point) = {
   val distance = dir.distance(pt)
   val distanceSqr = dir.distanceSqr(pt)
   val t1 = Delta.eq(distance, distanceSqr)
   assert(t1)
 }
 
 def nearestIsOn(dir: Dir, pt: Point) = {
   val near = dir.nearest(pt)
   val t1 = dir.through(near)
   println(dir)
   println(near)
   assert(t1)
 }

 def normalizedNormIs1(dir: Dir) = {
   val norm = dir.normalized.norm
   assert(Delta.eq(1, norm))
 }

 def normalIsNormal(dir: Dir) = {
   val normalDir = dir.normalDir
   val t1 = dir.normal(normalDir)
   assert(t1)
 }

 def normalIsPlus90Degree(dir: Dir) = {
   val normal = dir.normalDir
   val angle = dir.angleTo(normal)
   val normalized = mod(angle, 2*Math.PI)
   assert(Delta.eq(Math.PI/2, normalized))
 }

 def mod(d1: Double, d2: Double) = (d1 % d2 + d2) % d2

 def testCosAngle(dir1: Dir, dir2: Dir) = {
   val deg = dir1 angleTo dir2
   val cos = dir1 cosTo dir2
   assert(Delta.eq(cos, Math.cos(deg)))
 }

 def testSinAngle(dir1: Dir, dir2: Dir) = {
   val deg = dir1 angleTo dir2
   val sin = dir1 sinTo dir2
   assert(Delta.eq(sin, Math.sin(deg)))
 }

}
