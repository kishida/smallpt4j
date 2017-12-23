/*
 Copyright (c) 2017 Naoki Kishida (naokikishida@gmail.com / twitter: @kis)
This software is released under the MIT License.
( https://github.com/kishida/smallpt4j/blob/master/LICENSE.txt )

This is based on the smallpt( http://www.kevinbeason.com/smallpt/ )
that is released under the MIT License.
( https://github.com/kishida/smallpt4j/blob/master/smallpt_LICENSE.txt )
*/

package naoki.smallpt;

import static naoki.smallpt.SmallPT.Reflection.DIFFUSE;
import static org.apache.commons.math3.util.FastMath.abs;
import static org.apache.commons.math3.util.FastMath.asin;
import static org.apache.commons.math3.util.FastMath.atan2;
import static org.apache.commons.math3.util.FastMath.cos;
import static org.apache.commons.math3.util.FastMath.floor;
import static org.apache.commons.math3.util.FastMath.max;
import static org.apache.commons.math3.util.FastMath.min;
import static org.apache.commons.math3.util.FastMath.pow;
import static org.apache.commons.math3.util.FastMath.sin;
import static org.apache.commons.math3.util.FastMath.sqrt;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

import javax.imageio.ImageIO;

public class SmallPT {

    private static final int SAMPLES_DEFAULT = 40;

    static final class Vec {        // Usage: time ./smallpt 5000  xv image.ppm

        double x, y, z;                  // position, also color (r,g,b)

        public Vec(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        Vec() {
            this(0, 0, 0);
        }

        Vec add(Vec b) {
            return new Vec(x + b.x, y + b.y, z + b.z);
        }

        Vec sub(Vec b) {
            return new Vec(x - b.x, y - b.y, z - b.z);
        }

        Vec mul(double b) {
            return new Vec(x * b, y * b, z * b);
        }

        Vec vecmul(Vec b) {
            return new Vec(x * b.x, y * b.y, z * b.z);
        }

        Vec normalize() {
            double dist = sqrt(x * x + y * y + z * z);
            x /= dist;
            y /= dist;
            z /= dist;
            return this;
        }

        double dot(Vec b) {
            return x * b.x + y * b.y + z * b.z;
        } // cross:

        Vec mod(Vec b) {
            return new Vec(y * b.z - z * b.y, z * b.x - x * b.z, x * b.y - y * b.x);
        }
    }

    static final class Ray {

        final Vec obj, dist;

        public Ray(Vec o, Vec d) {
            this.obj = o;
            this.dist = d;
        }

    }

    static enum Reflection {
        DIFFUSE, SPECULAR, REFRECTION
    }  // material types, used in radiance()// material types, used in radiance()// material types, used in radiance()// material types, used in radiance()

    static abstract class Surface {
        final Vec pos;
        final Texture texture;

        public Surface(Vec pos, Texture texture) {
            this.pos = pos;
            this.texture = texture;
        }

        public Surface(Vec pos, Vec emission, Vec color, Reflection reflection) {
            this(pos, new SolidTexture(emission, color, reflection));
        }
        
        abstract double intersect(Ray y);
        abstract void position(Vec p, Ray r, Vec[] n, Col[] c);
        abstract Point makeXY(Vec p);
    }
    
    private static final double EPS = 1e-4;
    
    static final class Sphere extends Surface {

        final double rad;       // radius

        public Sphere(double rad, Vec p, Vec e, Vec c, Reflection refl) {
            super(p, e, c, refl);
            this.rad = rad;
        }
        public Sphere(double rad, Vec p, Texture texture) {
            super(p, texture);
            this.rad = rad;
        }

        @Override
        double intersect(Ray r) { // returns distance, 0 if nohit
            Vec op = pos.sub(r.obj); // Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
            double t,
                    b = op.dot(r.dist),
                    det = b * b - op.dot(op) + rad * rad;
            if (det < 0) {
                return 0;
            }
            det = sqrt(det);
            return (t = b - det) > EPS ? t : ((t = b + det) > EPS ? t : 0);
        }
        
        @Override
        void position(Vec x, Ray r, Vec[] n, Col[] c) {
            n[0] = x.sub(pos).normalize();
            c[0] = texture.getCol(this, x);
        }

        @Override
        Point makeXY(Vec x) {
            Vec position = x.sub(pos).mul(1 / rad);
            double phi = atan2(position.z, position.x);
            double theta = asin(position.y);
            return new Point(1 - (phi + Math.PI) / (2 * Math.PI), (theta + Math.PI / 2) / Math.PI);
        }
        
    }
    
    static final class Plane extends Surface {
        final double width, height;
        public Plane(double x, double y, Vec pos, Vec emission, Vec color, Reflection reflection) {
            super(pos, emission, color, reflection);
            this.width = x;
            this.height = y;
        }
        public Plane(double x, double y, Vec pos, Texture tex) {
            super(pos, tex);
            this.width = x;
            this.height = y;
        }
        @Override
        double intersect(Ray ray) {
            if (ray.dist.z < EPS && ray.dist.z > -EPS) {
                return 0;
            }
            double d = (pos.z - ray.obj.z) / ray.dist.z;
            if (d < 0) {
                return 0;
            }
            Vec x = ray.obj.add(ray.dist.mul(d));
            Vec p = x.sub(pos);
            if (p.x < 0 || p.x > width || p.y < 0 || p.y > height) {
                return 0;
            }
            if (!texture.isHit(this, x)) {
                return 0;
            }
            return d;
        }

        @Override
        void position(Vec p, Ray r, Vec[] n, Col[] c) {
            n[0] = new Vec(0, 0, r.dist.z > 0 ? -1 : 1);
            c[0] =  texture.getCol(this, p);
        }
        
        @Override
        Point makeXY(Vec p) {
            return new Point((p.x - pos.x) / width, (p.y - pos.y) / height);
        }
    }
    static final class Point {
        final double x, y;

        public Point(double x, double y) {
            this.x = x;
            this.y = y;
        }
    }
    static final class Col {
        final Vec emission, color;
        final Reflection reflection;

        public Col(Vec emission, Vec color,Reflection reflection) {
            this.emission = emission;
            this.color = color;
            this.reflection= reflection;
        }
    }
    static abstract class Texture {
        abstract Col getCol(Surface s, Vec x);
        boolean isHit(Surface s, Vec x)  {
            return true;
        }
    }
    static class SolidTexture extends Texture {
        final Col col;

        public SolidTexture(Vec emission, Vec color, Reflection ref) {
            this.col = new Col(emission, color, ref);
        }
        @Override
        Col getCol(Surface s, Vec x) {
            return col;
        }
    }
    static class CheckTexture extends Texture {
        final Col col1, col2;
        final double freq;

        public CheckTexture(Vec col1, Vec col2, double freq) {
            this.col1 = new Col(new Vec(), col1, DIFFUSE);
            this.col2 = new Col(new Vec(), col2, DIFFUSE);
            this.freq = freq;
        }

        @Override
        Col getCol(Surface s, Vec x) {
            Point p = s.makeXY(x);
            return (under(p.x / freq) - 0.5) * (under(p.y / freq) - 0.5) > 0 ? col1 : col2;
        }
        private double under(double d) {
            return d - floor(d);
        }
    }
    
    static class BitmapTexture extends Texture {
        final BufferedImage img;
        final int width, height;
        final Vec emission = new Vec();

        public BitmapTexture(String file) {
            try {
                img = ImageIO.read(SmallPT.class.getResourceAsStream(file));
                width = img.getWidth(null);
                height = img.getHeight(null);
            } catch (IOException ex) {
                throw new UncheckedIOException(ex);
            }
        }
        
        @Override
        Col getCol(Surface s, Vec x) {
            int rgb = getRgb(s, x);
            return new Col(emission, new Vec((rgb >> 16 & 255) / 255., (rgb >> 8 & 255) / 255., (rgb & 255) / 255.), DIFFUSE);
        }

        @Override
        boolean isHit(Surface s, Vec x) {
            int rgb = getRgb(s, x);
            return rgb >> 24 != 0;
        }
        
        int getRgb(Surface s, Vec x) {
            Point pos = s.makeXY(x);
            return img.getRGB((int)(pos.x * width), (int)((1 - pos.y) * height));
        }
    }
    
    static class EmissionTexture extends BitmapTexture {
        final Vec emission = new Vec(12, 12, 12);
        final Vec color = new Vec();
        public EmissionTexture(String file) {
            super(file);
        }

        @Override
        Col getCol(Surface s, Vec x) {
            return new Col(emission, color, DIFFUSE);
        }

        @Override
        boolean isHit(Surface s, Vec x) {
            int rgb = getRgb(s, x);
            return (rgb >> 24 != 0) && (rgb >> 16 & 255) < 80;
        }
        
    }
    
    static final Surface spheres[] = {//Scene: radius, position, emission, color, material
        new Sphere(1e5,  new Vec(1e5 + 1, 40.8, 81.6),   new Vec(), new Vec(.75, .25, .25), Reflection.DIFFUSE),//Left
        new Sphere(1e5,  new Vec(-1e5 + 99, 40.8, 81.6), new Vec(), new Vec(.25, .25, .75), Reflection.DIFFUSE),//Rght
        new Sphere(1e5,  new Vec(50, 40.8, 1e5),         new Vec(), new Vec(.75, .75, .75), Reflection.DIFFUSE),//Back
        new Sphere(1e5,  new Vec(50, 40.8, -1e5 + 170),  new Vec(), new Vec(), Reflection.DIFFUSE),//Frnt
        new Sphere(1e5,  new Vec(50, 1e5, 81.6),         new Vec(), new Vec(.75, .75, .75), Reflection.DIFFUSE),//Botm
        new Sphere(1e5,  new Vec(50, -1e5 + 81.6, 81.6), new Vec(), new Vec(.75, .75, .75), Reflection.DIFFUSE),//Top
        new Sphere(13, new Vec(27, 13, 47),          new Vec(), new Vec(1, 1, 1).mul(.999), Reflection.SPECULAR),//Mirr
        new Sphere(10, new Vec(73, 10, 78),          new Vec(), new Vec(1, 1, 1).mul(.999), Reflection.REFRECTION),//Glas
        new Sphere(600,  new Vec(50, 681.6 - .27, 81.6), new Vec(1, 1, 1), new Vec(), Reflection.DIFFUSE), //Lite
        new Plane(40, 30, new Vec(30, 0, 60), new BitmapTexture("/duke600px.png")),
        new Sphere(10, new Vec(17, 10, 85), new CheckTexture(new Vec(.25, .75, .25), new Vec(.95, .95, .95), .3)),
        new Plane(32, 24, new Vec(45, 0, 90), new EmissionTexture("/duke600px.png"))
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
    static boolean intersect(Ray r, double[] t, Surface[] robj) {
        t[0] = INF;
        for (int i = 0; i < spheres.length; ++i) {
            Surface obj = spheres[i];
            double d = obj.intersect(r);
            if (d != 0 && (d < t[0])) {
                t[0] = d;
                robj[0] = obj;
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
        Surface[] robj = {null};
        Vec[] rn = {null};
        Col[] rc = {null};
        if (!intersect(r, t, robj)) {
            return new Vec(); // if miss, return black
        }
        Surface obj = robj[0];        // the hit object
        Vec x = r.obj.add(r.dist.mul(t[0]));

        obj.position(x, r, rn, rc);
        Col tex = rc[0];
        Vec n = rn[0];
        Vec nl = n.dot(r.dist) < 0 ? n : n.mul(-1);
        Vec f = tex.color;
        double p = max(f.x, max(f.y, f.z)); // max refl
        depth++;
        if (depth > 5) {
            if (depth < 50 && getRandom() < p) {// 最大反射回数を設定
                f = f.mul(1 / p);
            } else {
                return tex.emission; //R.R.
            }
        }
        if (null == tex.reflection) {
            throw new IllegalStateException();
        } else switch(tex.reflection) {
            case DIFFUSE:
                double r1 = 2 * Math.PI * getRandom(),
                        r2 = getRandom(),
                        r2s = sqrt(r2);
                Vec w = nl,
                        u = ((abs(w.x) > .1 ? UNIT_Y : UNIT_X).mod(w)).normalize(),
                        v = w.mod(u);
                Vec d = (u.mul(cos(r1) * r2s).add(v.mul(sin(r1) * r2s)).add(w.mul(sqrt(1 - r2)))).normalize();
                return tex.emission.add(f.vecmul(radiance(new Ray(x, d), depth)));
            case SPECULAR:
                // Ideal SPECULAR reflection
                return tex.emission.add(f.vecmul(radiance(new Ray(x, r.dist.sub(n.mul(2 * n.dot(r.dist)))), depth)));
            case REFRECTION:
                Ray reflectionRay = new Ray(x, r.dist.sub(n.mul(2 * n.dot(r.dist))));     // Ideal dielectric REFRACTION
                boolean into = n.dot(nl) > 0;                // Ray from outside going in?
                double nc = 1,
                        nt = 1.5,
                        nnt = into ? nc / nt : nt / nc,
                        ddn = r.dist.dot(nl),
                        cos2t = 1 - nnt * nnt * (1 - ddn * ddn);
                if (cos2t < 0) { // Total internal reflection
                    return tex.emission.add(f.vecmul(radiance(reflectionRay, depth)));
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
                return tex.emission.add(f.vecmul(depth > 2 ? (getRandom() < probability // Russian roulette
                        ? radiance(reflectionRay, depth).mul(RP) : radiance(new Ray(x, tdir), depth).mul(TP))
                        : radiance(reflectionRay, depth).mul(Re).add(radiance(new Ray(x, tdir), depth).mul(Tr))));
            default:
                throw new IllegalStateException();
        }
    }

    public static void main(String... argv) throws IOException {
        int w = 1024,
                h = 768,
                samps = (argv.length > 0 ? Integer.parseInt(argv[0]) : SAMPLES_DEFAULT )/ 4; // # samples

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
                        c[i] = c[i].add(new Vec(clamp(r.x), clamp(r.y), clamp(r.z)).mul(.25));
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
            imagesource[i] = 255 << 24 | toInt(c[i].x) << 16 | toInt(c[i].y) << 8 | toInt(c[i].z);
        }
        BufferedImage out = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
        out.setRGB(0, 0, w, h, imagesource, 0, w);
        File f = new File("image.png");
        ImageIO.write(out, "png", f);

    }

}
