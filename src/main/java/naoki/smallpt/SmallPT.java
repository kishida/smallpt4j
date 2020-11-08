/*
 Copyright (c) 2017 Naoki Kishida (naokikishida@gmail.com / twitter: @kis)
This software is released under the MIT License.
( https://github.com/kishida/smallpt4j/blob/master/LICENSE.txt )

This is based on the smallpt( http://www.kevinbeason.com/smallpt/ )
that is released under the MIT License.
( https://github.com/kishida/smallpt4j/blob/master/smallpt_LICENSE.txt )
*/

package naoki.smallpt;

import static java.lang.Math.abs;
import static java.lang.Math.cos;
import static java.lang.Math.min;
import static java.lang.Math.pow;
import static java.lang.Math.sin;
import static java.lang.Math.sqrt;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;
import jdk.incubator.vector.*;

import javax.imageio.ImageIO;

public class SmallPT {

    private static final int SAMPLES_DEFAULT = 40;
    private static final VectorSpecies<Double> SPECIES = DoubleVector.SPECIES_256;
    
    static DoubleVector fromValues(double x, double y, double z) {
        return DoubleVector.fromArray(SPECIES, new double[]{x, y, z, 0}, 0);
    }
    
    static final class Vec {        

        private DoubleVector v;

        public Vec(double x, double y, double z) {
            v = fromValues(x, y, z);
        }

        Vec() {
            this(DoubleVector.zero(SPECIES));
        }
        
        Vec(DoubleVector v) {
            this.v = v;
        }

        Vec add(Vec b) {
            return new Vec(v.add(b.v));
        }

        Vec sub(Vec b) {
            return new Vec(v.sub(b.v));
        }

        Vec mul(double b) {
            return new Vec(v.mul(b));
        }

        Vec div(double b) {
            return new Vec(v.div(b));
        }
        
        Vec vecmul(Vec b) {
            return new Vec(v.mul(b.v));
        }

        Vec normalize() {
            double dist = sqrt(v.mul(v).reduceLanes(VectorOperators.ADD));
            return div(dist);
        }

        double dot(Vec b) {
            return v.mul(b.v).reduceLanes(VectorOperators.ADD);
        } // cross:

        Vec mod(Vec b) {
            return new Vec(
                    fromValues(getY(), getZ(), getX())
                        .mul(fromValues(b.getZ(), b.getX(), b.getY()))
                        .sub(fromValues(getZ(), getX(), getY())
                                .mul(fromValues(b.getY(), b.getZ(), b.getX()))));
        }
        
        double max() {
            return v.reduceLanes(VectorOperators.MAX);
        }

        public double getX() {
            return v.lane(0);
        }

        public double getY() {
            return v.lane(1);
        }

        public double getZ() {
            return v.lane(2);
        }
        
    }

    static final class Ray {

        final Vec obj, dist;

        public Ray(Vec o, Vec d) {
            this.obj = o;
            this.dist = d;
        }

    };

    private static enum Reflection {
        DIFFUSE, SPECULAR, REFRECTION
    };  // material types, used in radiance()// material types, used in radiance()// material types, used in radiance()// material types, used in radiance()

    static final class Sphere {

        final double rad;       // radius
        final Vec pos, emission, color;      // position, emission, color
        final Reflection reflection;      // reflection type (DIFFuse, SPECular, REFRactive)

        public Sphere(double rad, Vec p, Vec e, Vec c, Reflection refl) {
            this.rad = rad;
            this.pos = p;
            this.emission = e;
            this.color = c;
            this.reflection = refl;
        }

        double intersect(Ray r) { // returns distance, 0 if nohit
            Vec op = pos.sub(r.obj); // Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
            double t,
                    eps = 1e-4,
                    b = op.dot(r.dist),
                    det = b * b - op.dot(op) + rad * rad;
            if (det < 0) {
                return 0;
            } else {
                det = sqrt(det);
            }
            return (t = b - det) > eps ? t : ((t = b + det) > eps ? t : 0);
        }
    };
    static final Sphere spheres[] = {//Scene: radius, position, emission, color, material
        new Sphere(1e5,  new Vec(1e5 + 1, 40.8, 81.6),   new Vec(), new Vec(.75, .25, .25), Reflection.DIFFUSE),//Left
        new Sphere(1e5,  new Vec(-1e5 + 99, 40.8, 81.6), new Vec(), new Vec(.25, .25, .75), Reflection.DIFFUSE),//Rght
        new Sphere(1e5,  new Vec(50, 40.8, 1e5),         new Vec(), new Vec(.75, .75, .75), Reflection.DIFFUSE),//Back
        new Sphere(1e5,  new Vec(50, 40.8, -1e5 + 170),  new Vec(), new Vec(), Reflection.DIFFUSE),//Frnt
        new Sphere(1e5,  new Vec(50, 1e5, 81.6),         new Vec(), new Vec(.75, .75, .75), Reflection.DIFFUSE),//Botm
        new Sphere(1e5,  new Vec(50, -1e5 + 81.6, 81.6), new Vec(), new Vec(.75, .75, .75), Reflection.DIFFUSE),//Top
        new Sphere(16.5, new Vec(27, 16.5, 47),          new Vec(), new Vec(1, 1, 1).mul(.999), Reflection.SPECULAR),//Mirr
        new Sphere(16.5, new Vec(73, 16.5, 78),          new Vec(), new Vec(1, 1, 1).mul(.999), Reflection.REFRECTION),//Glas
        new Sphere(600,  new Vec(50, 681.6 - .27, 81.6), new Vec(12, 12, 12), new Vec(), Reflection.DIFFUSE) //Lite
    };

    static double clamp(double x) {
        return x < 0 ? 0 : x > 1 ? 1 : x;
    }

    static int toInt(double x) {
        return min(255, (int) (pow(clamp(x), 1 / 2.2) * 255 + .5));
    }
    private static final double INF = 1e20;
    private static final Vec UNIT_X = new Vec(1, 0, 0);
    private static final Vec UNIT_Y = new Vec(0, 1, 0);
    static boolean intersect(Ray r, double[] t, int[] id) {
        t[0] = INF;
        for (int i = 0; i < spheres.length; ++i) {
            double d = spheres[i].intersect(r);
            if (d != 0 && (d < t[0])) {
                t[0] = d;
                id[0] = i;
            }
        }
        return t[0] < INF;
    }

    private static double getRandom() {
        return ThreadLocalRandom.current().nextDouble();
    }
    
    static Vec radiance(Ray r, int depth) {
        double[] t = {0};                               // distance to intersection
        int[] id = {0};                               // id of intersected object
        if (!intersect(r, t, id)) {
            return new Vec(); // if miss, return black
        }
        Sphere obj = spheres[id[0]];        // the hit object
        Vec x = r.obj.add(r.dist.mul(t[0]));

        Vec n = x.sub(obj.pos).normalize();
        Vec nl = n.dot(r.dist) < 0 ? n : n.mul(-1);
        Vec f = obj.color;
        depth++;
        if (depth > 5) {
            double p = f.max(); // max refl
            if (depth < 50 && getRandom() < p) {// 最大反射回数を設定
                f = f.mul(1 / p);
            } else {
                return obj.emission; //R.R.
            }
        }
        if (null == obj.reflection) {
            throw new IllegalStateException();
        } else switch (obj.reflection) {
            case DIFFUSE:
                double r1 = 2 * Math.PI * getRandom(),
                        r2 = getRandom(),
                        r2s = sqrt(r2);
                Vec w = nl,
                        u = ((abs(w.getX()) > .1 ? UNIT_Y : UNIT_X).mod(w)).normalize(),
                        v = w.mod(u);
                Vec d = (u.mul(cos(r1) * r2s).add(v.mul(sin(r1) * r2s)).add(w.mul(sqrt(1 - r2)))).normalize();
                return obj.emission.add(f.vecmul(radiance(new Ray(x, d), depth)));
            case SPECULAR:
                // Ideal SPECULAR reflection
                return obj.emission.add(f.vecmul(radiance(new Ray(x, r.dist.sub(n.mul(2 * n.dot(r.dist)))), depth)));
            case REFRECTION:
                Ray reflectionRay = new Ray(x, r.dist.sub(n.mul(2 * n.dot(r.dist))));     // Ideal dielectric REFRACTION
                boolean into = n.dot(nl) > 0;                // Ray from outside going in?
                double nc = 1,
                        nt = 1.5,
                        nnt = into ? nc / nt : nt / nc,
                        ddn = r.dist.dot(nl),
                        cos2t = 1 - nnt * nnt * (1 - ddn * ddn);
                if (cos2t < 0) { // Total internal reflection
                    return obj.emission.add(f.vecmul(radiance(reflectionRay, depth)));
                }
                Vec tdir = (r.dist.mul(nnt).sub(n.mul((into ? 1 : -1) * (ddn * nnt + sqrt(cos2t))))).normalize();
                double a = nt - nc,
                        b = nt + nc,
                        R0 = a * a / (b * b),
                        c = 1 - (into ? -ddn : tdir.dot(n));
                double Re = R0 + (1 - R0) * c * c * c * c * c,
                        Tr = 1 - Re,
                        probability = .25 + .5 * Re,
                        RP = Re / probability,
                        TP = Tr / (1 - probability);
                return obj.emission.add(f.vecmul(depth > 2 ? (getRandom() < probability // Russian roulette
                        ? radiance(reflectionRay, depth).mul(RP) : radiance(new Ray(x, tdir), depth).mul(TP))
                        : radiance(reflectionRay, depth).mul(Re).add(radiance(new Ray(x, tdir), depth).mul(Tr))));
            default:
                throw new IllegalStateException();
        }
    }

    public static void main(String... argv) throws IOException {
        int w = 1024,
                h = 768,
                samps = (argv.length == 2 ? Integer.parseInt(argv[1]) : SAMPLES_DEFAULT )/ 4; // # samples

        Ray cam = new Ray(new Vec(50, 52, 295.6), new Vec(0, -0.042612, -1).normalize()); // cam pos, dir
        Vec cx = new Vec(w * .5135 / h, 0, 0),
                cy = (cx.mod(cam.dist)).normalize().mul(.5135);

        Instant start = Instant.now();
        Vec[] c = new Vec[w * h];
        Arrays.fill(c, new Vec());

        AtomicInteger count = new AtomicInteger();
        IntStream.range(0, h).parallel().forEach(y -> {
            //for (int y = 0; y < h; ++y) {
            System.out.printf("Rendering (%d spp) %5.2f%%%n", samps * 4, 100. * count.getAndIncrement() / (h - 1));
            for (int x = 0; x < w; x++) {// Loop cols
                int i = (h - y - 1) * w + x;
                for (int sy = 0; sy < 2; sy++) { // 2x2 subpixel rows
                    for (int sx = 0; sx < 2; sx++) {        // 2x2 subpixel cols
                        Vec r = new Vec();
                        for (int s = 0; s < samps; s++) {
                            double r1 = 2 * getRandom(),
                                    dx = r1 < 1 ? sqrt(r1) - 1 : 1 - sqrt(2 - r1);
                            double r2 = 2 * getRandom(),
                                    dy = r2 < 1 ? sqrt(r2) - 1 : 1 - sqrt(2 - r2);
                            Vec d = cx.mul(((sx + .5 + dx) / 2 + x) / w - .5)
                                    .add(cy.mul(((sy + .5 + dy) / 2 + y) / h - .5)).add(cam.dist);
                            r = r.add(radiance(new Ray(cam.obj.add(d.mul(140)), d.normalize()), 0));
                        } // Camera rays are pushed ^^^^^ forward to start in interior
                        r = r.mul(1. / samps);
                        c[i] = c[i].add(new Vec(clamp(r.getX()), clamp(r.getY()), clamp(r.getZ())).mul(.25));
                    }
                }
            }
        });

        Instant finish = Instant.now();
        System.out.printf("Samples:%d Type:%s Time:%s%n",
                samps * 4,
                "master",
                Duration.between(start, finish));
        int[] imagesource = new int[w * h];
        for (int i = 0; i < w * h; ++i) {
            imagesource[i] = 255 << 24 | toInt(c[i].getX()) << 16 | toInt(c[i].getY()) << 8 | toInt(c[i].getZ());
        }
        BufferedImage out = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
        out.setRGB(0, 0, w, h, imagesource, 0, w);
        File f = new File("image.png");
        ImageIO.write(out, "png", f);

    }

}
