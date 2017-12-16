package com.github.kmizu.smallpt

import java.lang.Math.cos
import java.lang.Math.sin
import java.lang.Math.sqrt
import java.awt.Image
import java.awt.Toolkit
import java.awt.image.BufferedImage
import java.awt.image.MemoryImageSource
import java.io.File
import java.io.IOException
import java.util
import java.util.{Arrays, Random}
import java.util.concurrent.atomic.AtomicInteger
import java.util.stream.IntStream
import javax.imageio.ImageIO

object SmallPT {
  case class Vec(var x: Double, var y: Double, var z: Double) {
    def this() {
      this(0, 0, 0)
    }

    def add(b: Vec) = Vec(x + b.x, y + b.y, z + b.z)

    def sub(b: Vec) = Vec(x - b.x, y - b.y, z - b.z)

    def mul(b: Double) = Vec(x * b, y * b, z * b)

    def mult(b: Vec) = Vec(x * b.x, y * b.y, z * b.z)

    def normalize = {
      val dist = Math.sqrt(x * x + y * y + z * z)
      x /= dist
      y /= dist
      z /= dist
      this
    }

    def dot(b: Vec): Double = x * b.x + y * b.y + z * b.z

    def mod(b: Vec): Vec = {
      Vec(y * b.z - z * b.y, z * b.x - x * b.z, x * b.y - y * b.x)
    }
  }

  case class Ray(var obj: Vec, var dist: Vec)

  object Reflection extends Enumeration {
    type Reflection = Value
    val DIFF, SPEC, REFR = Value
  }

  case class Sphere(var rad: Double, // radius
                    var pos: Vec,
                    var emission: Vec,
                    var color: Vec, // position, emission, color
                    var reflection: Reflection.Reflection // reflection type (DIFFuse, SPECular, REFRactive)
                   ) {
    def intersect(r: Ray): Double = { // returns distance, 0 if nohit
      val op = pos.sub(r.obj)
      // Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
      var t = .0
      val eps = 1e-4
      val b = op.dot(r.dist)
      var det = b * b - op.dot(op) + rad * rad
      if (det < 0) return 0
      else det = Math.sqrt(det)
      if (({t = b - det; t}) > eps) t
      else if ({t = b + det; t} > eps) {
        t
      }
      else {
        0
      }
    }
  }

  val spheres = Array(//Scene: radius, position, emission, color, material
    Sphere(1e5, Vec(1e5 + 1, 40.8, 81.6), new Vec, Vec(.75, .25, .25), Reflection.DIFF), //Left
    Sphere(1e5, Vec(-1e5 + 99, 40.8, 81.6), new Vec, Vec(.25, .25, .75), Reflection.DIFF), //Rght
    Sphere(1e5, Vec(50, 40.8, 1e5), new Vec, Vec(.75, .75, .75), Reflection.DIFF), //Back
    Sphere(1e5, Vec(50, 40.8, -1e5 + 170), new Vec, new Vec, Reflection.DIFF), //Frnt
    Sphere(1e5, Vec(50, 1e5, 81.6), new Vec, Vec(.75, .75, .75), Reflection.DIFF), //Botm
    Sphere(1e5, Vec(50, -1e5 + 81.6, 81.6), new Vec, Vec(.75, .75, .75), Reflection.DIFF), //Top
    Sphere(16.5, Vec(27, 16.5, 47), new Vec, Vec(1, 1, 1).mul(.999), Reflection.SPEC), //Mirr
    Sphere(16.5, Vec(73, 16.5, 78), new Vec, Vec(1, 1, 1).mul(.999), Reflection.REFR), //Glas
    Sphere(600, Vec(50, 681.6 - .27, 81.6), Vec(12, 12, 12), new Vec, Reflection.DIFF) //Lite
  )

  def clamp(x: Double): Double = {
    if (x < 0) 0
    else if (x > 1) 1
    else x
  }

  def toInt(x: Double) = {
    Math.min(255, (Math.pow(clamp(x), 1 / 2.2) * 255 + .5).toInt)
  }

  val inf = 1e20

  def intersect(r: Ray, t: Array[Double], id: Array[Int]): Boolean = {
    t(0) = inf
    var i = 0
    while (i < spheres.length) {
      val d = spheres(i).intersect(r)
      if (d != 0 && (d < t(0))) {
        t(0) = d
        id(0) = i
      }

      i += 1
    }
    t(0) < inf
  }

  val rnd = new Random

  def radiance(r: Ray, depthInit: Int): Vec = {
    var depth = depthInit
    val t: Array[Double] = Array(0)
    // distance to intersection
    val id: Array[Int] = Array(0) // id of intersected object
    if (!intersect(r, t, id)) return new Vec // if miss, return black
    val obj = spheres(id(0))
    // the hit object
    val x = r.obj.add(r.dist.mul(t(0)))
    val n = x.sub(obj.pos).normalize
    val nl = if (n.dot(r.dist) < 0) n
    else n.mul(-1)
    var f = obj.color
    val p = Math.max(f.x, Math.max(f.y, f.z)) // max refl
    depth += 1
    if (depth > 5) if (depth < 50 && rnd.nextDouble < p) { // 最大反射回数を設定
      f = f.mul(1 / p)
    }
    else return obj.emission //R.R.
    if (null == obj.reflection) throw new IllegalStateException
    else obj.reflection match {
      case Reflection.DIFF =>
        val r1 = 2 * Math.PI * rnd.nextDouble
        val r2 = rnd.nextDouble
        val r2s = sqrt(r2)
        val w = nl
        val u = (if (Math.abs(w.x) > .1) {
          Vec(0, 1, 0)
        }
        else {
          Vec(1, 0, 0)
        }).mod(w).normalize
        val v = w.mod(u)
        val d = u.mul(cos(r1) * r2s).add(v.mul(sin(r1) * r2s)).add(w.mul(sqrt(1 - r2))).normalize
        obj.emission.add(f.mult(radiance(Ray(x, d), depth)))
      case Reflection.SPEC =>
        // Ideal SPECULAR reflection
        obj.emission.add(f.mult(radiance(Ray(x, r.dist.sub(n.mul(2 * n.dot(r.dist)))), depth)))
      case Reflection.REFR =>
        val reflectionRay = Ray(x, r.dist.sub(n.mul(2 * n.dot(r.dist))))
        // Ideal dielectric REFRACTION
        val into = n.dot(nl) > 0
        // Ray from outside going in?
        val nc = 1
        val nt = 1.5
        val nnt = if (into) nc / nt
        else nt / nc
        val ddn = r.dist.dot(nl)
        val cos2t = 1 - nnt * nnt * (1 - ddn * ddn)
        if (cos2t < 0) { // Total internal reflection
          return obj.emission.add(f.mult(radiance(reflectionRay, depth)))
        }
        val tdir = r.dist.mul(nnt).sub(n.mul((if (into) {
          1
        }
        else {
          -(1)
        }) * (ddn * nnt + sqrt(cos2t)))).normalize
        val a = nt - nc
        val b = nt + nc
        val R0 = a * a / (b * b)
        val c = 1 - (if (into) -ddn
        else tdir.dot(n))
        val Re = R0 + (1 - R0) * c * c * c * c * c
        val Tr = 1 - Re
        val probability =.25 +.5 * Re
        val RP = Re / probability
        val TP = Tr / (1 - probability)
        obj.emission.add(f.mult(if (depth > 2) if (rnd.nextDouble < probability) { // Russian roulette
          radiance(reflectionRay, depth).mul(RP)
        }
        else {
          radiance(Ray(x, tdir), depth).mul(TP)
        }
        else radiance(reflectionRay, depth).mul(Re).add(radiance(Ray(x, tdir), depth).mul(Tr))))
      case _ =>
        throw new IllegalStateException
    }
  }

  def main(argv: Array[String]): Unit = {
    val f = new File("image.png")
    System.out.println(f.getAbsolutePath)
    val w = 1024
    val h = 768
    val samps = if (argv.length == 2) argv(1).toInt / 4
    else 2
    // # samples
    val cam = Ray(Vec(50, 52, 295.6), Vec(0, -0.042612, -1).normalize)
    // cam pos, dir
    val cx = Vec(w * .5135 / h, 0, 0)
    val cy = cx.mod(cam.dist).normalize.mul(.5135)
    val c = new Array[Vec](w * h)
    Arrays.fill(c.asInstanceOf[Array[AnyRef]], new Vec)
    val count = new AtomicInteger
    IntStream.range(0, h).parallel.forEach{y =>
      def foo(y: Int) = {
        printf("Rendering (%d spp) %5.2f%%%n", samps * 4, 100.0 * count.getAndIncrement / (h - 1))
        var x = 0
        while (x < w) { // Loop cols
          val i = (h - y - 1) * w + x
          var sy = 0
          while (sy < 2) { // 2x2 subpixel rows
            var sx = 0
            while (sx < 2) { // 2x2 subpixel cols
              var r = new Vec
              var s = 0
              while (s < samps) {
                val r1 = 2 * rnd.nextDouble
                val dx = if (r1 < 1) sqrt(r1) - 1
                else 1 - sqrt(2 - r1)
                val r2 = 2 * rnd.nextDouble
                val dy = if (r2 < 1) sqrt(r2) - 1
                else 1 - sqrt(2 - r2)
                val d = cx.mul(((sx + .5 + dx) / 2 + x) / w - .5).add(cy.mul(((sy + .5 + dy) / 2 + y) / h - .5)).add(cam.dist)
                r = r.add(radiance(Ray(cam.obj.add(d.mul(140)), d.normalize), 0))
                // Camera rays are pushed ^^^^^ forward to start in interior
                s += 1
              }
              r = r.mul(1.0 / samps)
              c(i) = c(i).add(new Vec(clamp(r.x), clamp(r.y), clamp(r.z)).mul(.25))
              sx += 1
            }
            sy += 1
          }
          x += 1
        }
      }

      foo(y)
    }
    val imagesource = new Array[Int](w * h)
    var i = 0
    while (i < w * h) {
      imagesource(i) = 255 << 24 | toInt(c(i).x) << 16 | toInt(c(i).y) << 8 | toInt(c(i).z)

      i += 1
    }
    val image = Toolkit.getDefaultToolkit.createImage(new MemoryImageSource(w, h, imagesource, 0, w))
    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    out.createGraphics.drawImage(image, 0, 0, null)
    ImageIO.write(out, "png", f)
  }
}
